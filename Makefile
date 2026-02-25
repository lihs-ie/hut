.DEFAULT_GOAL := help

DOCKER_COMPOSE_FILE := applications/docker/docker-compose.yaml
ENV ?=
TF_DIR = infrastructure/environments/$(ENV)

define check_env
	@if [ "$(ENV)" != "stg" ] && [ "$(ENV)" != "prd" ]; then \
		echo "Error: ENV must be 'stg' or 'prd'. Usage: make $@ ENV=stg"; \
		exit 1; \
	fi
endef

define check_gcp_env
	@if [ -z "$(GCP_PROJECT_ID)" ]; then \
		echo "Error: GCP_PROJECT_ID is not set"; \
		exit 1; \
	fi
	@if [ -z "$(GCP_REGION)" ]; then \
		echo "Error: GCP_REGION is not set"; \
		exit 1; \
	fi
	@if [ -z "$(CLOUD_RUN_READER_SERVICE)" ]; then \
		echo "Error: CLOUD_RUN_READER_SERVICE is not set"; \
		exit 1; \
	fi
	@if [ -z "$(CLOUD_RUN_ADMIN_SERVICE)" ]; then \
		echo "Error: CLOUD_RUN_ADMIN_SERVICE is not set"; \
		exit 1; \
	fi
endef

# ==============================================================================
# Setup
# ==============================================================================

.PHONY: setup
setup: ## Initial project setup
	bash ./scripts/setup.sh

.PHONY: init-local
init-local: ## Initialize local development environment
	bash ./scripts/init-local.sh

# ==============================================================================
# Frontend
# ==============================================================================

.PHONY: dev
dev: ## Start local development (emulator + app)
	pnpm dev

.PHONY: dev-reader
dev-reader: ## Start reader app (port 3000)
	pnpm dev:reader

.PHONY: dev-admin
dev-admin: ## Start admin app (port 3001)
	pnpm dev:admin

.PHONY: build
build: ## Build all frontend packages
	pnpm build

.PHONY: lint
lint: ## Lint all frontend packages
	pnpm lint

.PHONY: test
test: ## Run all tests with vitest
	pnpm test

.PHONY: typecheck
typecheck: ## Run TypeScript type checking
	pnpm typecheck

.PHONY: e2e
e2e: ## Run E2E tests
	pnpm e2e

.PHONY: storybook
storybook: ## Start Storybook (port 6006)
	pnpm storybook

.PHONY: seed
seed: ## Seed development data
	pnpm seed

# ==============================================================================
# Emulator
# ==============================================================================

.PHONY: emulator-up
emulator-up: ## Start Firebase emulator containers
	docker compose -f $(DOCKER_COMPOSE_FILE) up

.PHONY: emulator-up-detach
emulator-up-detach: ## Start Firebase emulator containers (detached)
	docker compose -f $(DOCKER_COMPOSE_FILE) up -d

.PHONY: emulator-down
emulator-down: ## Stop Firebase emulator containers
	docker compose -f $(DOCKER_COMPOSE_FILE) down

.PHONY: emulator-logs
emulator-logs: ## Follow Firebase emulator logs
	docker compose -f $(DOCKER_COMPOSE_FILE) logs -f

.PHONY: emulator-build
emulator-build: ## Rebuild Firebase emulator containers (no cache)
	docker compose -f $(DOCKER_COMPOSE_FILE) build --no-cache

.PHONY: emulator-clean
emulator-clean: ## Remove Firebase emulator data
	rm -rf applications/docker/frontend/data/*

# ==============================================================================
# Terraform (requires ENV=stg|prd)
# ==============================================================================

.PHONY: tf-init
tf-init: ## Initialize Terraform (ENV=stg|prd)
	$(call check_env)
	terraform -chdir=$(TF_DIR) init

.PHONY: tf-plan
tf-plan: ## Preview Terraform changes (ENV=stg|prd)
	$(call check_env)
	terraform -chdir=$(TF_DIR) plan

.PHONY: tf-apply
tf-apply: ## Apply Terraform changes (ENV=stg|prd)
	$(call check_env)
	terraform -chdir=$(TF_DIR) apply

.PHONY: tf-destroy
tf-destroy: ## Destroy Terraform resources (ENV=stg|prd)
	$(call check_env)
	terraform -chdir=$(TF_DIR) destroy

# ==============================================================================
# Cloud Run Proxy (requires GCP environment variables)
# ==============================================================================

.PHONY: proxy
proxy: ## Proxy to both Cloud Run services (background)
	$(call check_gcp_env)
	@echo "Starting proxy for reader and admin..."
	@echo "  - Reader: http://localhost:8080"
	@echo "  - Admin:  http://localhost:8081"
	@gcloud run services proxy $(CLOUD_RUN_READER_SERVICE) --region $(GCP_REGION) --project $(GCP_PROJECT_ID) --port 8080 & \
	gcloud run services proxy $(CLOUD_RUN_ADMIN_SERVICE) --region $(GCP_REGION) --project $(GCP_PROJECT_ID) --port 8081 & \
	wait

# ==============================================================================
# CI
# ==============================================================================

.PHONY: ci
ci: lint build test ## Run CI pipeline (lint + build + test)

# ==============================================================================
# Help
# ==============================================================================

.PHONY: help
help: ## Show this help message
	@echo "Usage: make [target]"
	@echo ""
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-20s\033[0m %s\n", $$1, $$2}'
