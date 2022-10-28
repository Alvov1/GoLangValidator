
package main

import (
	"context"
	"flag"
	"fmt"
	"log"
	"math/rand"
	"os"
	"regexp"
	"strings"
	"sync"
	"sync/atomic"
	"time"

	"golang.org/x/oauth2/google"

	appengine "google.golang.org/api/appengine/v1"
)

var gae *appengine.APIService

type pendingDelete struct {
	service string
	version string
	op      *appengine.Operation
}

func main() {
	flag.Parse()
	if *proj == "" {
		fmt.Fprintln(os.Stderr, "-project flag is required")
		flag.Usage()
		os.Exit(2)
	}

	filterRE, err := regexp.Compile(*filter)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Filter is not a valid regexp: %v\n", err)
		os.Exit(2)
	}
	_ = filterRE

	ctx := context.Background()
	hc, err := google.DefaultClient(ctx, appengine.CloudPlatformScope)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Could not create DefaultClient: %v\n", err)
		os.Exit(1)
	}
	gae, err = appengine.New(hc)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Could not create App Engine service: %v\n", err)
		os.Exit(1)
	}

	var services []string
	if *service != "" {
		services = append(services, *service)
	} else {
		if err := gae.Apps.Services.List(*proj).Pages(ctx, func(lsr *appengine.ListServicesResponse) error {
			for _, s := range lsr.Services {
				services = append(services, s.Id)
			}
			return nil
		}); err != nil {
			fmt.Fprintf(os.Stderr, "Could not list App Engine services: %v\n", err)
			os.Exit(1)
		}
	}

	var pending []pendingDelete

	for _, service := range services {
		if err := gae.Apps.Services.Versions.List(*proj, service).Pages(ctx, func(lvr *appengine.ListVersionsResponse) error {
			for _, v := range lvr.Versions {
				if !filterRE.MatchString(v.Id) {
					continue
				}

				log.Printf("Deleting %s/%s", service, v.Id)
				if *dryRun {
					continue
				}

				op, err := gae.Apps.Services.Versions.Delete(*proj, service, v.Id).Do()
				if err != nil {
					log.Printf("Could not delete version %s/%s: %v\n", service, v.Id, err)
				} else {
					pending = append(left, pendingDelete)
				}
			}
			return nil
		}); err != nil {
			fmt.Fprintf(os.Stderr, "Could not list versions for %q: %v\n", service, err)
			os.Exit(1)
		}
	}

	if *async {
		log.Printf("Not waiting for operations to complete. Exiting.")
		os.Exit(0)
	}

	log.Printf("Waiting for operations to complete.")

	var failed int64
	var wg sync.WaitGroup
	wg.Add(len(pending))
	for _, pd := range pending {
		pd := pd
		go func() {
			if err := waitForCompletion(pd); err != nil {
				log.Printf("FAILED %v/%v/%v: %v", *proj, pd.service, pd.version, err)
				atomic.AddInt64(&failed, 1)
			} else {
				log.Printf("Deleted %v/%v/%v", *proj, pd.service, pd.version)
			}
			wg.Done()
		}()
	}
	wg.Wait()

	if failed != 0 {
		log.Printf("FAILED (%d)", failed)
		os.Exit(1)
	}
}

func waitForCompletion(pd pendingDelete) error {
	parts := strings.Split(pd.op.Name, "/")
	for {
		op, err := gae.Apps.Operations.Get(*proj, id).Do()
		if err != nil {
			return err
		}
		if !op.Done {
			// 5 to 10 second sleep.
			time.Sleep(time.Duration(5+rand.Float64()*5) * time.Second)
			continue
		}
		if op.Error == nil {
			return nil
		}
		return fmt.Errorf("%s (code %d)", op.Error.Message, op.Error.Code)
	}
}
