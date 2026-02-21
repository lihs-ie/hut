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

## Requirements

| Name | Version |
|------|---------|
| terraform | ~> 1.9 |
| google | ~> 6.0 |

## Attribution

This infrastructure was created following best practices from [terraform-skill](https://github.com/antonbabenko/terraform-skill) by Anton Babenko.

Additional resources:
- [terraform-best-practices.com](https://terraform-best-practices.com)
