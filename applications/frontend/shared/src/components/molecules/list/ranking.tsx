import { SectionHeader } from "@shared/components/atoms/text/section";
import { RankingRow } from "@shared/components/atoms/list/ranking";
import styles from "./ranking.module.css";

type Props = {
  title: string;
  description?: string;
  items: Array<{
    label: string;
    value: number;
    subLabel?: string;
  }>;
  limit?: number;
  valueFormatter?: (value: number) => string;
};

export const RankingTable = (props: Props) => {
  const items = props.items.slice(0, props.limit ?? 10);
  const maxValue =
    items.length > 0 ? Math.max(...items.map((item) => item.value)) : 0;

  return (
    <div className={styles.container}>
      <SectionHeader title={props.title} description={props.description} />
      <div className={styles.list}>
        {items.map((item, index) => (
          <RankingRow
            key={item.label}
            rank={index + 1}
            label={item.label}
            value={
              props.valueFormatter
                ? props.valueFormatter(item.value)
                : item.value
            }
            subLabel={item.subLabel}
            maxValue={maxValue}
          />
        ))}
      </div>
    </div>
  );
};
