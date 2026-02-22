# GitHub Actions IAM Module

Workload Identity Federation (WIF) and deployer service account for GitHub Actions CI/CD.

## Usage

```hcl
module "github_actions_iam" {
  source = "../../modules/github_actions_iam"

  project_id        = var.project_id
  github_repository = "owner/repo"
}
```

## Resources Created

- `google_iam_workload_identity_pool` - WIF pool for GitHub Actions
- `google_iam_workload_identity_pool_provider` - OIDC provider (issuer: `https://token.actions.githubusercontent.com`)
- `google_service_account` - Deployer service account
- `google_project_iam_member` - IAM role bindings for the deployer
- `google_service_account_iam_member` - WIF binding for the deployer

## Inputs

| Name | Description | Type | Default | Required |
|------|-------------|------|---------|:--------:|
| project\_id | GCP project ID | `string` | n/a | yes |
| github\_repository | GitHub repository (`owner/repo`) | `string` | n/a | yes |
| pool\_id | Workload Identity Pool ID | `string` | `"github-actions"` | no |
| provider\_id | WIF Provider ID | `string` | `"github-oidc"` | no |
| service\_account\_id | Deployer service account ID | `string` | `"github-deployer"` | no |
| deployer\_roles | IAM roles for the deployer | `list(string)` | `["roles/run.developer", "roles/artifactregistry.writer", "roles/iam.serviceAccountUser"]` | no |

## Outputs

| Name | Description |
|------|-------------|
| workload\_identity\_provider | Full resource name of the WIF provider |
| service\_account\_email | Email of the deployer service account |
