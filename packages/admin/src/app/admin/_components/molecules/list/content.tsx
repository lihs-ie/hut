import { Props as Content } from "./card/content";
import { AdminContentListPresenter } from "./content.presenter";

export type Props = {
  contents: Content[];
};

export const AdminContentList = (props: Props) => (
  <AdminContentListPresenter contents={props.contents} />
);
