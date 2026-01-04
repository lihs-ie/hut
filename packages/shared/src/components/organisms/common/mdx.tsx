import { MDXRemote } from "next-mdx-remote/rsc";
import { MDXRemoteProps } from "next-mdx-remote/rsc";
import rehypePrettyCode from "rehype-pretty-code";

export type Props = {
  content: MDXRemoteProps["source"];
};

const prettyCodeOptions = {
  theme: "github-dark",
  keepBackground: true,
};

export const MDXRenderer = (props: Props) => (
  <div>
    <MDXRemote
      source={props.content}
      options={{
        mdxOptions: {
          rehypePlugins: [[rehypePrettyCode, prettyCodeOptions]],
        },
      }}
    />
  </div>
);
