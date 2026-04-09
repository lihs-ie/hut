import { ImageResponse } from "next/og";
import { findBySlug } from "@/actions/article";

export const runtime = "nodejs";
export const size = { width: 1200, height: 630 };
export const contentType = "image/png";

type Props = {
  params: Promise<{ slug: string }>;
};

export default async function Image(props: Props) {
  const { slug } = await props.params;
  const article = await findBySlug(slug);

  return new ImageResponse(
    (
      <div
        style={{
          width: "100%",
          height: "100%",
          display: "flex",
          flexDirection: "column",
          justifyContent: "flex-end",
          padding: "60px",
          background: "#000000",
          color: "#ffffff",
        }}
      >
        <div
          style={{
            fontSize: "14px",
            fontWeight: 900,
            letterSpacing: "0.2em",
            textTransform: "uppercase",
            color: "#ef4444",
            marginBottom: "24px",
          }}
        >
          Article
        </div>
        <div
          style={{
            fontSize: "48px",
            fontWeight: 900,
            lineHeight: 1.2,
            color: "#ffffff",
            marginBottom: "40px",
            maxWidth: "900px",
          }}
        >
          {article.title}
        </div>
        <div
          style={{
            fontSize: "20px",
            fontWeight: 700,
            color: "#a1a1aa",
          }}
        >
          hut
        </div>
      </div>
    ),
    { ...size },
  );
}
