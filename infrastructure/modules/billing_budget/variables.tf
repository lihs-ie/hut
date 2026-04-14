variable "billing_account_id" {
  description = "GCP billing account ID that owns the budget (e.g. 01ABCD-234567-89EFGH)"
  type        = string

  validation {
    condition     = can(regex("^[A-Z0-9-]+$", var.billing_account_id))
    error_message = "The billing_account_id must consist of uppercase letters, digits, and hyphens."
  }
}

variable "display_name" {
  description = "Human-readable name for the budget shown in the GCP console"
  type        = string

  validation {
    condition     = length(var.display_name) > 0 && length(var.display_name) <= 60
    error_message = "The display_name must be between 1 and 60 characters."
  }
}

variable "budget_amount_jpy" {
  description = "Monthly budget amount in Japanese yen (JPY). Must be a positive whole number because JPY has no fractional unit."
  type        = number

  validation {
    condition     = var.budget_amount_jpy > 0
    error_message = "The budget_amount_jpy must be greater than zero."
  }

  validation {
    condition     = floor(var.budget_amount_jpy) == var.budget_amount_jpy
    error_message = "The budget_amount_jpy must be a whole number (JPY has no fractional unit)."
  }
}

variable "notification_email" {
  description = "Email address that receives budget alert notifications"
  type        = string
  sensitive   = true

  validation {
    condition     = can(regex("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$", var.notification_email))
    error_message = "The notification_email must be a valid email address."
  }
}

variable "threshold_percents" {
  description = "Budget threshold fractions that trigger alerts (e.g. 0.5 means 50 percent of the budget)"
  type        = list(number)
  default     = [0.5, 0.8, 1.0, 1.2]

  validation {
    condition     = length(var.threshold_percents) > 0
    error_message = "At least one threshold percent must be provided."
  }

  validation {
    condition = alltrue([
      for threshold in var.threshold_percents : threshold > 0
    ])
    error_message = "Every threshold_percents value must be greater than zero."
  }
}

variable "project_id" {
  description = "GCP project ID that the budget filter monitors"
  type        = string
}

variable "labels" {
  description = "Labels to apply to the notification channel"
  type        = map(string)
  default     = {}
}
