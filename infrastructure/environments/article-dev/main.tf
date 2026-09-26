locals {
  queue_names = {
    excerpt_generation     = "hut-article-excerpt-generation-dev"
    excerpt_generation_dlq = "hut-article-excerpt-generation-dlq-dev"
    excerpt_completion     = "hut-article-excerpt-completion-dev"
    excerpt_completion_dlq = "hut-article-excerpt-completion-dlq-dev"
  }
}

resource "cloudflare_queue" "article" {
  for_each = local.queue_names

  account_id = var.cloudflare_account_id
  queue_name = each.value
}
