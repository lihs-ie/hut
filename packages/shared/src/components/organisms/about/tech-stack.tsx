import { TechStackList } from "@shared/components/molecules/list/tech-stack";
import { Tag } from "@shared/domains/attributes/tag";
import { Profile } from "@shared/domains/user";

export type Props = {
  getProfile: () => Promise<Profile>;
  getAllTags: () => Promise<Tag[]>;
  now: Date;
};

export const TechStackSection = async (props: Props) => {
  const [profile, tags] = await Promise.all([
    props.getProfile(),
    props.getAllTags(),
  ]);

  const tagMap = new Map(tags.map((tag) => [tag.identifier, tag]));

  const resolvedStacks = new Map<
    string,
    { from: Date; name: string; logo: string }[]
  >();

  profile.techStacks.forEach((stacks, category) => {
    resolvedStacks.set(
      category,
      stacks.map((stack) => {
        const tag = tagMap.get(stack.tag);
        return {
          from: stack.from,
          name: tag?.name ?? "",
          logo: tag?.logo ?? "",
        };
      })
    );
  });

  return <TechStackList techStacks={resolvedStacks} now={props.now} />;
};
