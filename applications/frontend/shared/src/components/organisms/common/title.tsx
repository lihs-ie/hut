import { Timeline } from "@shared/domains/common";

import { TitlePresenter } from "./title.presenter";

export type Props<T> = {
  titleOf: (content: T) => string;
  timelineOf: (content: T) => Timeline;
  find: (slug: string) => Promise<T>;
  slug: string;
};

export const Title = async <T,>(props: Props<T>) => {
  const content = await props.find(props.slug);

  return (
    <TitlePresenter
      title={props.titleOf(content)}
      timeline={props.timelineOf(content)}
    />
  );
};
