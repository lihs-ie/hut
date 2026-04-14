# Infrastructure

Terraform configurations for the HUT application infrastructure.

## Directory Structure

```
infrastructure/
  environments/
    stg/          STG environment (GCP project: hut-stg)
    prd/          PRD environment (GCP project: hut-prd)
  modules/
    artifact_registry/    Docker image repository
    cloudrun_service/     Cloud Run v2 service
    eventarc_trigger/     Eventarc Pub/Sub trigger
    firebase_storage/     Firebase Storage (GCS) bucket
    firestore/            Firestore database
    iam/                  Service accounts and IAM bindings
    pubsub/               Pub/Sub topics and subscriptions
    secret_manager/       Secret Manager secrets and access
```

## Environments

| Environment | GCP Project | Description |
|-------------|-------------|-------------|
| STG | hut-stg | Staging environment. All services on Cloud Run. |
| PRD | hut-prd | Production environment. Reader on Cloudflare Workers, admin and workers on Cloud Run. |

## Naming Conventions

STG environment services use `stg-` prefix as defined in the design specification (Section 4.1). PRD services do not use a prefix. Each environment operates in a dedicated GCP project, providing resource isolation.

## Terraform Management Scope

The following components are managed outside Terraform:

| Component | Management Method | Reason |
|-----------|-------------------|--------|
| Cloud Build | GCP Console / gcloud CLI | Build triggers require repository integration; low change frequency after initial setup |
| Cloudflare Workers | Wrangler CLI (CI/CD) | OpenNext build artifacts are deployed via Wrangler |
| Cloudflare DNS | Cloudflare Dashboard | Managed via Terraform after custom domain acquisition |
| Cloudflare WAF | Cloudflare Dashboard | Managed via Terraform after custom domain acquisition |

## ISR Revalidation Webhook Operation

Admin Cloud Run notifies Reader to revalidate ISR caches via a shared secret. The secret (`*-revalidation-secret`) is stored in Secret Manager and injected into Cloud Run services as `REVALIDATION_SECRET`.

| Environment | Secret Manager secret | Accessors | Reader endpoint source |
|-------------|------------------------|-----------|------------------------|
| STG | `stg-revalidation-secret` | `hut-stg-reader` SA, `hut-stg-admin` SA | `module.cloudrun_reader.service_uri` (auto-wired) |
| PRD | `prd-revalidation-secret` | `hut-prd-admin` SA only (Reader runs on Cloudflare Workers) | `var.reader_endpoint` (manual injection) |

### Initial secret value population

After `terraform apply` creates the empty secret shells, populate each secret version manually (Terraform must not hold secret material):

```bash
# STG
printf 'REPLACE_WITH_STRONG_RANDOM_64_HEX' | \
  gcloud secrets versions add stg-revalidation-secret --project=<stg-project-id> --data-file=-

# PRD
printf 'REPLACE_WITH_STRONG_RANDOM_64_HEX' | \
  gcloud secrets versions add prd-revalidation-secret --project=<prd-project-id> --data-file=-
```

Use the same value for the Cloudflare Workers Reader binding in PRD:

```bash
wrangler secret put REVALIDATION_SECRET --env production
```

### `reader_endpoint` variable (PRD only)

In PRD the Reader is not a Cloud Run service, so its URL cannot be referenced from the Terraform graph. Provide the Cloudflare Workers URL via `reader_endpoint` in `secrets.auto.tfvars` (not committed) or via `TF_VAR_reader_endpoint` in CI:

```hcl
reader_endpoint = "https://reader.example.com"
```

The value must be an `https://` URL without a trailing slash. STG wires the reader endpoint automatically via the `cloudrun_reader` module output and does not accept a `reader_endpoint` variable.

## Requirements

| Name | Version |
|------|---------|
| terraform | ~> 1.9 |
| google | ~> 6.0 |

## Attribution

This infrastructure was created following best practices from [terraform-skill](https://github.com/antonbabenko/terraform-skill) by Anton Babenko.

Additional resources:
- [terraform-best-practices.com](https://terraform-best-practices.com)
