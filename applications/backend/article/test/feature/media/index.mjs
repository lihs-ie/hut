const availableImage = "01ARZ3NDEKTSV4RRFFQ69G5FA1";
const inspectingImage = "01ARZ3NDEKTSV4RRFFQ69G5FA2";

export default {
  async fetch(request) {
    const path = new URL(request.url).pathname;
    if (!request.headers.get("X-Hut-Actor") ||
        !request.headers.get("X-Correlation-Identifier")) {
      return new Response(null, { status: 400 });
    }
    if (request.method === "POST" && path === "/images/availability") {
      const body = await request.json();
      if (!Array.isArray(body.images)) {
        return new Response(null, { status: 400 });
      }
      return Response.json({
        available: body.images.filter((identifier) => identifier === availableImage),
      });
    }
    const match = /^\/images\/([0-9A-HJKMNP-TV-Z]{26})$/.exec(
      path,
    );
    const imageIdentifier = match?.[1];
    if (request.method !== "GET") {
      return new Response(null, { status: 405 });
    }
    if (imageIdentifier !== availableImage && imageIdentifier !== inspectingImage) {
      return new Response(null, { status: 404 });
    }
    return Response.json({
      imageIdentifier,
      state: imageIdentifier === availableImage ? "available" : "inspecting",
    });
  },
};
