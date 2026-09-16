# Media remote dev environment

This Terraform root manages only the Cloudflare resources for the Media remote
development environment. It does not manage GCP resources or Worker scripts.

## State backend bootstrap

Create the `hut-terraform-state` R2 bucket and an Object Read & Write token scoped
to that bucket before initializing this root. Supply credentials and the
account-specific R2 endpoint through environment variables:

```sh
export AWS_ACCESS_KEY_ID="..."
export AWS_SECRET_ACCESS_KEY="..."
export AWS_ENDPOINT_URL_S3="https://<ACCOUNT_ID>.r2.cloudflarestorage.com"
export CLOUDFLARE_API_TOKEN="..."

terraform -chdir=infrastructure/environments/dev init
```

The backend stores state at `media/dev/terraform.tfstate`. Credentials must not
be passed with `-backend-config` because Terraform persists backend configuration
under `.terraform/` and may include it in plan files.

## Plan and apply

Create a local `terraform.tfvars` from `terraform.tfvars.example`, then run:

```sh
terraform -chdir=infrastructure/environments/dev plan
terraform -chdir=infrastructure/environments/dev apply
```

The Cloudflare token must be limited to the resources this root manages. R2
backend credentials are separate and scoped only to `hut-terraform-state`.

## GitHub Environment

Create the GitHub Environment `dev` and configure these secrets:

- `CLOUDFLARE_ACCOUNT_ID`
- `CLOUDFLARE_TERRAFORM_API_TOKEN`
- `CLOUDFLARE_WORKER_DEPLOY_API_TOKEN`
- `TERRAFORM_STATE_R2_ACCESS_KEY_ID`
- `TERRAFORM_STATE_R2_SECRET_ACCESS_KEY`
- `R2_SIGNING_ACCESS_KEY_ID`
- `R2_SIGNING_SECRET_ACCESS_KEY`
- `CLOUDFLARE_CACHE_PURGE_TOKEN`

Configure `CLOUDFLARE_ZONE_IDENTIFIER` as an Environment variable. Keep the
Terraform, Wrangler, state, upload-signing, and cache-purge credentials
separate. Scope the upload-signing credentials to
`hut-media-tmp-uploads-dev`, and scope the purge token to the `lihs-dev.com`
zone.

Set an account-wide USD 1 Budget Alert in the Cloudflare dashboard. The alert
is informational and does not stop or delete resources.

## Deploy

Run `Media Dev Infrastructure` first when bootstrapping the environment. It
creates the resources required before the upload-signing credentials can be
scoped to `hut-media-tmp-uploads-dev`.

Run the `Media Dev Deploy` workflow manually. It tests the code, provisions
resources, applies forward-only D1 migrations, deploys consumer Workers before
the API Worker, and verifies the control plane. Worker deployment failures are
rolled back to the previous Worker versions. Terraform and D1 are deliberately
not rolled back.

The Media API has no public route, `workers.dev` URL, or preview URL. Until the
Admin Worker is connected through a Service Binding, remote verification is
limited to the control plane; no public probe Worker is created.
