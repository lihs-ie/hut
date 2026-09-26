export default {
  async fetch(request, environment) {
    const url = new URL(request.url);
    if (url.pathname !== "/article-do" && url.pathname !== "/article-api") {
      return new Response(null, { status: 404 });
    }

    const target = new URL(`https://article.internal${url.searchParams.get("path") ?? ""}`);
    const articleRequest = new Request(target, {
      method: request.method,
      headers: request.headers,
      body: request.method === "POST" ? await request.arrayBuffer() : undefined,
    });
    if (url.pathname === "/article-api") {
      return environment.ARTICLE_API.fetch(articleRequest);
    }
    return environment.ARTICLE_DO.getByName("articles").fetch(articleRequest);
  },
};
