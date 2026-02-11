import { describe, it, expect } from "vitest";
import {
  ImmutableList,
  immutableListSchema,
} from "@shared/domains/common/collections/list/common";
import z from "zod";

describe("domains/common/collections/list", () => {
  describe("ImmutableList", () => {
    describe("コンストラクタ", () => {
      it("配列からImmutableListを作成できる", () => {
        const list = ImmutableList([1, 2, 3]);

        expect(list.size()).toBe(3);
        expect(list.toArray()).toEqual([1, 2, 3]);
      });

      it("空の配列からImmutableListを作成できる", () => {
        const list = ImmutableList([]);

        expect(list.size()).toBe(0);
        expect(list.isEmpty()).toBe(true);
      });

      it("引数なしで空のImmutableListを作成できる", () => {
        const list = ImmutableList<number>();

        expect(list.size()).toBe(0);
        expect(list.isEmpty()).toBe(true);
      });
    });

    describe("静的メソッド", () => {
      describe("fromArray", () => {
        it("配列からImmutableListを作成できる", () => {
          const list = ImmutableList.fromArray([1, 2, 3]);

          expect(list.toArray()).toEqual([1, 2, 3]);
        });
      });

      describe("of", () => {
        it("可変引数からImmutableListを作成できる", () => {
          const list = ImmutableList.of(1, 2, 3);

          expect(list.toArray()).toEqual([1, 2, 3]);
        });

        it("引数なしで空のリストを作成できる", () => {
          const list = ImmutableList.of<number>();

          expect(list.isEmpty()).toBe(true);
        });
      });

      describe("empty", () => {
        it("空のImmutableListを作成できる", () => {
          const list = ImmutableList.empty<number>();

          expect(list.size()).toBe(0);
          expect(list.isEmpty()).toBe(true);
        });
      });

      describe("isList", () => {
        it("ImmutableListを正しく判定する", () => {
          const list = ImmutableList([1, 2, 3]);

          expect(ImmutableList.isList(list)).toBe(true);
        });

        it("配列はfalseを返す", () => {
          expect(ImmutableList.isList([1, 2, 3])).toBe(false);
        });

        it("nullはfalseを返す", () => {
          expect(ImmutableList.isList(null)).toBe(false);
        });

        it("undefinedはfalseを返す", () => {
          expect(ImmutableList.isList(undefined)).toBe(false);
        });
      });
    });

    describe("サイズと状態", () => {
      describe("size", () => {
        it("正しいサイズを返す", () => {
          const list = ImmutableList([1, 2, 3, 4, 5]);

          expect(list.size()).toBe(5);
        });
      });

      describe("isEmpty", () => {
        it("空のリストでtrueを返す", () => {
          const list = ImmutableList<number>();

          expect(list.isEmpty()).toBe(true);
        });

        it("要素があるリストでfalseを返す", () => {
          const list = ImmutableList([1]);

          expect(list.isEmpty()).toBe(false);
        });
      });

      describe("isNotEmpty", () => {
        it("要素があるリストでtrueを返す", () => {
          const list = ImmutableList([1]);

          expect(list.isNotEmpty()).toBe(true);
        });

        it("空のリストでfalseを返す", () => {
          const list = ImmutableList<number>();

          expect(list.isNotEmpty()).toBe(false);
        });
      });
    });

    describe("要素の追加", () => {
      describe("addFirst", () => {
        it("先頭に要素を追加できる", () => {
          const list = ImmutableList([2, 3]);
          const newList = list.addFirst(1);

          expect(newList.toArray()).toEqual([1, 2, 3]);
          // 元のリストは不変
          expect(list.toArray()).toEqual([2, 3]);
        });
      });

      describe("addFirstAll", () => {
        it("先頭に複数の要素を追加できる", () => {
          const list = ImmutableList([3, 4]);
          const newList = list.addFirstAll(1, 2);

          expect(newList.toArray()).toEqual([1, 2, 3, 4]);
        });
      });

      describe("addLast", () => {
        it("末尾に要素を追加できる", () => {
          const list = ImmutableList([1, 2]);
          const newList = list.addLast(3);

          expect(newList.toArray()).toEqual([1, 2, 3]);
          // 元のリストは不変
          expect(list.toArray()).toEqual([1, 2]);
        });
      });

      describe("addLastAll", () => {
        it("末尾に複数の要素を追加できる", () => {
          const list = ImmutableList([1, 2]);
          const newList = list.addLastAll(3, 4);

          expect(newList.toArray()).toEqual([1, 2, 3, 4]);
        });
      });
    });

    describe("要素の削除", () => {
      describe("remove", () => {
        it("要素を削除できる", () => {
          const list = ImmutableList([1, 2, 3]);
          const newList = list.remove(2);

          expect(newList.toArray()).toEqual([1, 3]);
          // 元のリストは不変
          expect(list.toArray()).toEqual([1, 2, 3]);
        });

        it("存在しない要素の削除は元のリストを返す", () => {
          const list = ImmutableList([1, 2, 3]);
          const newList = list.remove(4);

          expect(newList.toArray()).toEqual([1, 2, 3]);
        });

        it("最初に見つかった要素のみ削除する", () => {
          const list = ImmutableList([1, 2, 2, 3]);
          const newList = list.remove(2);

          expect(newList.toArray()).toEqual([1, 2, 3]);
        });
      });
    });

    describe("要素の取得", () => {
      describe("get", () => {
        it("インデックスで要素を取得できる", () => {
          const list = ImmutableList(["a", "b", "c"]);

          expect(list.get(0).get()).toBe("a");
          expect(list.get(1).get()).toBe("b");
          expect(list.get(2).get()).toBe("c");
        });

        it("範囲外のインデックスでは値が存在しない", () => {
          const list = ImmutableList([1, 2, 3]);

          expect(list.get(10).isPresent()).toBe(false);
          expect(list.get(-1).isPresent()).toBe(false);
        });
      });

      describe("first", () => {
        it("先頭の要素を取得できる", () => {
          const list = ImmutableList([1, 2, 3]);

          expect(list.first().get()).toBe(1);
        });

        it("空のリストでは値が存在しない", () => {
          const list = ImmutableList<number>();

          expect(list.first().isPresent()).toBe(false);
        });
      });

      describe("last", () => {
        it("末尾の要素を取得できる", () => {
          const list = ImmutableList([1, 2, 3]);

          expect(list.last().get()).toBe(3);
        });

        it("空のリストでは値が存在しない", () => {
          const list = ImmutableList<number>();

          expect(list.last().isPresent()).toBe(false);
        });
      });

      describe("find", () => {
        it("条件に合う要素を見つける", () => {
          const list = ImmutableList([1, 2, 3, 4, 5]);
          const result = list.find((value) => value > 3);

          expect(result.get()).toBe(4);
        });

        it("条件に合う要素がない場合は値が存在しない", () => {
          const list = ImmutableList([1, 2, 3]);
          const result = list.find((value) => value > 10);

          expect(result.isPresent()).toBe(false);
        });
      });

      describe("findIndex", () => {
        it("条件に合う要素のインデックスを返す", () => {
          const list = ImmutableList([1, 2, 3, 4, 5]);
          const index = list.findIndex((value) => value === 3);

          expect(index).toBe(2);
        });

        it("条件に合う要素がない場合は-1を返す", () => {
          const list = ImmutableList([1, 2, 3]);
          const index = list.findIndex((value) => value === 10);

          expect(index).toBe(-1);
        });
      });
    });

    describe("変換操作", () => {
      describe("map", () => {
        it("各要素を変換できる", () => {
          const list = ImmutableList([1, 2, 3]);
          const mapped = list.map((value) => value * 2);

          expect(mapped.toArray()).toEqual([2, 4, 6]);
        });

        it("インデックスを使用できる", () => {
          const list = ImmutableList(["a", "b", "c"]);
          const mapped = list.map((value, index) => `${index}:${value}`);

          expect(mapped.toArray()).toEqual(["0:a", "1:b", "2:c"]);
        });
      });

      describe("filter", () => {
        it("条件に合う要素のみ残す", () => {
          const list = ImmutableList([1, 2, 3, 4, 5]);
          const filtered = list.filter((value) => value % 2 === 0);

          expect(filtered.toArray()).toEqual([2, 4]);
        });

        it("すべての要素が条件に合わない場合は空を返す", () => {
          const list = ImmutableList([1, 3, 5]);
          const filtered = list.filter((value) => value % 2 === 0);

          expect(filtered.isEmpty()).toBe(true);
        });
      });

      describe("reduce", () => {
        it("要素を集約できる", () => {
          const list = ImmutableList([1, 2, 3, 4, 5]);
          const sum = list.reduce((acc, value) => acc + value, 0);

          expect(sum).toBe(15);
        });

        it("空のリストでは初期値を返す", () => {
          const list = ImmutableList<number>();
          const sum = list.reduce((acc, value) => acc + value, 100);

          expect(sum).toBe(100);
        });
      });

      describe("zip", () => {
        it("2つのリストを結合できる", () => {
          const list1 = ImmutableList([1, 2, 3]);
          const list2 = ImmutableList(["a", "b", "c"]);
          const zipped = list1.zip(list2);

          expect(zipped.toArray()).toEqual([
            [1, "a"],
            [2, "b"],
            [3, "c"],
          ]);
        });

        it("短い方の長さに合わせる", () => {
          const list1 = ImmutableList([1, 2, 3, 4, 5]);
          const list2 = ImmutableList(["a", "b"]);
          const zipped = list1.zip(list2);

          expect(zipped.toArray()).toEqual([
            [1, "a"],
            [2, "b"],
          ]);
        });
      });

      describe("reverse", () => {
        it("要素を逆順にできる", () => {
          const list = ImmutableList([1, 2, 3]);
          const reversed = list.reverse();

          expect(reversed.toArray()).toEqual([3, 2, 1]);
          // 元のリストは不変
          expect(list.toArray()).toEqual([1, 2, 3]);
        });
      });

      describe("sort", () => {
        it("要素をソートできる", () => {
          const list = ImmutableList([3, 1, 4, 1, 5, 9, 2, 6]);
          const sorted = list.sort((a, b) => a - b);

          expect(sorted.toArray()).toEqual([1, 1, 2, 3, 4, 5, 6, 9]);
        });

        it("元のリストは不変", () => {
          const list = ImmutableList([3, 1, 2]);
          list.sort((a, b) => a - b);

          expect(list.toArray()).toEqual([3, 1, 2]);
        });
      });

      describe("drop", () => {
        it("先頭からn個の要素を削除できる", () => {
          const list = ImmutableList([1, 2, 3, 4, 5]);
          const dropped = list.drop(2);

          expect(dropped.toArray()).toEqual([3, 4, 5]);
        });

        it("0個の場合は元のリストを返す", () => {
          const list = ImmutableList([1, 2, 3]);
          const dropped = list.drop(0);

          expect(dropped.toArray()).toEqual([1, 2, 3]);
        });

        it("負の数の場合は元のリストを返す", () => {
          const list = ImmutableList([1, 2, 3]);
          const dropped = list.drop(-1);

          expect(dropped.toArray()).toEqual([1, 2, 3]);
        });

        it("サイズより大きい数の場合は空を返す", () => {
          const list = ImmutableList([1, 2, 3]);
          const dropped = list.drop(10);

          expect(dropped.isEmpty()).toBe(true);
        });
      });
    });

    describe("イテレーション", () => {
      describe("foreach", () => {
        it("各要素に対してコールバックを実行する", () => {
          const list = ImmutableList([1, 2, 3]);
          const result: number[] = [];

          list.foreach((value) => result.push(value));

          expect(result).toEqual([1, 2, 3]);
        });

        it("インデックスを使用できる", () => {
          const list = ImmutableList(["a", "b", "c"]);
          const result: string[] = [];

          list.foreach((value, index) => result.push(`${index}:${value}`));

          expect(result).toEqual(["0:a", "1:b", "2:c"]);
        });
      });
    });

    describe("比較と検証", () => {
      describe("equals", () => {
        it("同じ内容のリストは等しい", () => {
          const list1 = ImmutableList([1, 2, 3]);
          const list2 = ImmutableList([1, 2, 3]);

          expect(list1.equals(list2)).toBe(true);
        });

        it("異なる内容のリストは等しくない", () => {
          const list1 = ImmutableList([1, 2, 3]);
          const list2 = ImmutableList([1, 2, 4]);

          expect(list1.equals(list2)).toBe(false);
        });

        it("異なるサイズのリストは等しくない", () => {
          const list1 = ImmutableList([1, 2, 3]);
          const list2 = ImmutableList([1, 2]);

          expect(list1.equals(list2)).toBe(false);
        });

        it("カスタム比較関数を使用できる", () => {
          const list1 = ImmutableList([{ id: 1 }, { id: 2 }]);
          const list2 = ImmutableList([{ id: 1 }, { id: 2 }]);

          expect(list1.equals(list2, (a, b) => a.id === b.id)).toBe(true);
        });
      });

      describe("exists", () => {
        it("条件に合う要素が存在する場合はtrueを返す", () => {
          const list = ImmutableList([1, 2, 3, 4, 5]);

          expect(list.exists((value) => value > 3)).toBe(true);
        });

        it("条件に合う要素が存在しない場合はfalseを返す", () => {
          const list = ImmutableList([1, 2, 3]);

          expect(list.exists((value) => value > 10)).toBe(false);
        });
      });

      describe("forall", () => {
        it("すべての要素が条件を満たす場合はtrueを返す", () => {
          const list = ImmutableList([2, 4, 6, 8]);

          expect(list.forall((value) => value % 2 === 0)).toBe(true);
        });

        it("条件を満たさない要素がある場合はfalseを返す", () => {
          const list = ImmutableList([2, 4, 5, 8]);

          expect(list.forall((value) => value % 2 === 0)).toBe(false);
        });

        it("空のリストではtrueを返す", () => {
          const list = ImmutableList<number>();

          expect(list.forall((value) => value > 0)).toBe(true);
        });
      });
    });

    describe("条件付き操作", () => {
      describe("when", () => {
        it("条件がtrueの場合はコールバックを実行する", () => {
          const list = ImmutableList([1, 2, 3]);
          const result = list.when(true, (self) => self.addLast(4));

          expect(result.toArray()).toEqual([1, 2, 3, 4]);
        });

        it("条件がfalseの場合は元のリストを返す", () => {
          const list = ImmutableList([1, 2, 3]);
          const result = list.when(false, (self) => self.addLast(4));

          expect(result.toArray()).toEqual([1, 2, 3]);
        });

        it("関数を条件として使用できる", () => {
          const list = ImmutableList([1, 2, 3]);
          const result = list.when(
            () => list.size() > 2,
            (self) => self.addLast(4)
          );

          expect(result.toArray()).toEqual([1, 2, 3, 4]);
        });
      });
    });
  });

  describe("immutableListSchema", () => {
    const numberListSchema = immutableListSchema(z.number());

    it("有効なImmutableListを検証できる", () => {
      const list = ImmutableList([1, 2, 3]);
      const result = numberListSchema.safeParse(list);

      expect(result.success).toBe(true);
    });

    it("要素の型が異なる場合はエラー", () => {
      const list = ImmutableList(["a", "b", "c"]);
      const result = numberListSchema.safeParse(list);

      expect(result.success).toBe(false);
    });

    it("nullは無効", () => {
      const result = numberListSchema.safeParse(null);

      expect(result.success).toBe(false);
    });

    it("undefinedは無効", () => {
      const result = numberListSchema.safeParse(undefined);

      expect(result.success).toBe(false);
    });

    it("通常の配列は無効", () => {
      const result = numberListSchema.safeParse([1, 2, 3]);

      expect(result.success).toBe(false);
    });
  });
});
