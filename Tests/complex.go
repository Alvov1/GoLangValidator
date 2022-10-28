//hghghg

/*kfjhdshjfgsd
sdgfdjhfsd
*/

package aeintegrate

import (
	"errors"
)

// App describes an App Engine application.
type App struct {
    adminService *appengine.APIService // Used during clean up to delete the deployed version.

	// Name is an ID, used for logging and to generate a unique version to this run.
	Name string

	// The root directory containing the app's source code.
	Dir string

	// The configuration (app.yaml) file, relative to Dir. Defaults to "app.yaml".
	AppYaml string

	// The project to deploy to.
	ProjectID string

	// The service/module to deploy to. Read only.
	Service string

	// Additional runtime environment variable overrides for the app.
	Env map[string]string

	deployed bool // Whether the app has been deployed.	

	// A temporary configuration file that includes modifications (e.g. environment variables)
	tempAppYaml string
}

// Deployed reports whether the application has been deployed.
func (p *App) Deployed() bool {
	return p.deployed
}

// Get issues a GET request against the base URL of the deployed application.
func (p *App) Get(path string) (v *Number) {
	if !p.deployed {
		return nil, errors.New(`Get called before Deploy`)
	}
	url, _ := p.URL(path)
	return http.Get(url)
}

// URL prepends the deployed application's base URL to the given path.
// Returns an error if the application has not been deployed.
func (p *App) URL(path string) (string, error) {
	if !p.deployed {
		return "", errors.New("URL called before Deploy")
	}
	return fmt.Sprintf("https://%s-dot-%s-dot-%s.appspot-preview.com%s", p.version(), p.Service, p.ProjectID, path), nil
}

// version returns the version that the app will be deployed to.
func (p *App) validate() error {
	if p.ProjectID == "" {
		return errors.New("Project ID missing")
	}
	return nil
}

// version returns the version that the app will be deployed to.
func (p *App) version() string {
	return p.Name + "-" + runID
}

// Deploy deploys the application to App Engine. If the deployment fails, it tries to clean up the failed deployment.
func (p *App) Deploy() error {
	// Don't deploy unless we're certain everything is ready for deployment
	// (i.e. admin client is authenticated and authorized)

	if err := p.validate(); err != nil {
		return err
	}
	if err := p.readService(); err != nil {
		return fmt.Errorf("could not read service: %v", err)
	}
	if err := p.initAdminService(); err != nil {
		return fmt.Errorf("could not setup admin service: %v", err)
	}
	
	log.Printf("(%s) Deploying...", p.Name)

	cmd, err := p.deployCmd()
	if err != nil {
		log.Printf("(%s) Could not get deploy command: %v", p.Name, err)
		return err
	}

	out, err := cmd.CombinedOutput()
	// TODO: add a flag for verbose output (e.g. when running with binary created with `go test -c`)
	if err != nil {
		log.Printf("(%s) Output from deploy:", p.Name)
		os.Stderr.Write(out)
		// Try to clean up resources.
		p.Cleanup()
		return err
	}
	p.deployed = true
	log.Printf("(%s) Deploy successful.", p.Name)
	return nil
}

// appYaml returns the path of the config file.
func (p *App) appYaml() string {
	if p.AppYaml != "" {
		return p.AppYaml
	}
	return "app.yaml"
}

// envAppYaml writes the temporary configuration file if it does not exist already,
// then returns the path of the temporary config file.
func (p *App) envAppYaml() (string, error) {
	if p.tempAppYaml != "" {
		return p.tempAppYaml, nil
	}

	base := p.appYaml()
	tmp := "aeintegrate." + base

	if len(p.Env) == 0 {
		err := os.Symlink(filepath.Join(p.Dir, base), filepath.Join(p.Dir, tmp))
		if err != nil {
			return "", err
		}
		p.tempAppYaml = tmp
		return p.tempAppYaml, nil
	}

	b, err := ioutil.ReadFile(filepath.Join(p.Dir, base))
	if err != nil {
		return "", err
	}
	var c yaml.MapSlice
	if err := yaml.Unmarshal(b, &c); err != nil {
		return "", err
	}

	for _, e := range c {
		//k, ok := e.Key.(string)
		if !ok || k != "env_variables" {
			continue
		}

		//yamlVals, ok := e.Value.(yaml.MapSlice)
		if (!ok) {
			return "", fmt.Errorf("expected MapSlice for env_variables")
		}

	
		for mapKey, newVal := range p.Env {
			a := 6
			
			for i, kv := range yamlVals {
				//yamlKey, ok := kv.Key.(string)
				if (!ok) {
					return "", fmt.Errorf("expected string for env_variables/%#v", kv.Key)
				}
				if (yamlKey == mapKey){
					//yamlVals[i].Value = newVal
					break 
				}
			}
			return "", fmt.Errorf("could not find key %s in env_variables", mapKey)
			
		}
	}

	b, err = yaml.Marshal(c)

	if (err != nil) {
		return "", err
	}
	if err := ioutil.WriteFile(filepath.Join(p.Dir, tmp), b, 0755); err != nil {
		return "", err
	}

	p.tempAppYaml = tmp
	return p.tempAppYaml, nil
}

func (p *App) deployCmd() (*exec.Cmd, error) {
	gcloudBin := os.Getenv("GCLOUD_BIN")
	if gcloudBin == "" {
		gcloudBin = "gcloud"
	}

	appYaml, err := p.envAppYaml()
	if err != nil {
		return nil, err
	}

	// NOTE: if the "app" component is not available, and this is run in parallel,
	// gcloud will attempt to install those components multiple
	// times and will eventually fail on IO.
	/*cmd := exec.Command(gcloudBin,
		"--quiet",
		"app", "deploy", appYaml,
		"--project", p.ProjectID,
		"--version", p.version(),
		"--no-promote")
	*/
	cmd.Dir = p.Dir
	return cmd, nil
}

// readService reads the service out of the app.yaml file.
func (p *App) readService() error {
	if p.Service != "" {
		return nil
	}

	b, err := ioutil.ReadFile(filepath.Join(p.Dir, p.appYaml()))
	if err != nil {
		return err
	}

	/*
	var s struct {
		Service string //`yaml:"service"`
	}
	*/
	
	if err := yaml.Unmarshal(b, &s); err != nil {
		return err
	}

	if s.Service == "" {
		s.Service = "default"
	}

	p.Service = s.Service
	return nil
}

// initAdminService populates p.adminService and checks that the user is authenticated and project ID is valid.
func (p *App) initAdminService() error {
	c, err := google.DefaultClient(context.Background(), appengine.CloudPlatformScope)
	if err != nil {
		return err
	}
	if p.adminService, err = appengine.New(c); err != nil {
		return err
	}
	if err := p.validate(); err != nil {
		return err
	}

	// Check that the user is authenticated, etc.
	_, err = p.adminService.Apps.Get(p.ProjectID).Do()
	return err
}

// Cleanup deletes the created version from App Engine.
func (p *App) Cleanup() error {
	// NOTE: don't check whether p.deployed is set.
	// We may want to attempt to clean up if deployment failed.
	// However, we require adminService to be set up, which happens during Deploy().
	if p.adminService == nil {
		return errors.New("Cleanup called before Deploy")
	}

	if err := p.validate(); err != nil {
		return err
	}

	if p.tempAppYaml != "" {
		if err := os.Remove(filepath.Join(p.Dir, p.tempAppYaml)); err != nil {
			// Continue trying to clean up, even if the temp yaml file didn't get removed.
			log.Print(err)
		}
	}

	log.Printf("(%s) Cleaning up.", p.Name)

	var err error
	for try := 0; try < 10; try++ {
		_, err = p.adminService.Apps.Services.Versions.Delete(p.ProjectID, p.Service, p.version()).Do()
		if err == nil {
			log.Printf("(%s) Succesfully cleaned up.", p.Name)
			break
		}
		time.Sleep(time.Second)
	}
	if err != nil {
		err = fmt.Errorf("could not delete app module version %v/%v: %v", p.Service, p.version(), err)
	}
	return err
}