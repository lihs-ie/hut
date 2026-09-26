const availableImage = "01ARZ3NDEKTSV4RRFFQ69G5FA1";
const inspectingImage = "01ARZ3NDEKTSV4RRFFQ69G5FA2";

export default {
  fetch(request) {
    const match = /^\/images\/([0-9A-HJKMNP-TV-Z]{26})$/.exec(
      new URL(request.url).pathname,
    );
    const imageIdentifier = match?.[1];
    if (request.method !== "GET") {
      return new Response(null, { status: 405 });
    }
    if (!request.headers.get("X-Hut-Actor") ||
        !request.headers.get("X-Correlation-Identifier")) {
      return new Response(null, { status: 400 });
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
