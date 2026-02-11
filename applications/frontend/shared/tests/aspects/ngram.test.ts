import { describe, it, expect } from "vitest";
import { generateNgrams, score, type Scorer } from "@shared/aspects/ngram";

describe("aspects/ngram", () => {
  describe("generateNgrams", () => {
    describe("基本的なN-gram生成", () => {
      it("英語テキストからN-gramを生成できる", () => {
        const ngrams = generateNgrams("hello");

        // "hello" → 正規化後 "hello" (5文字)
        // 2-gram: "he", "el", "ll", "lo" (4つ)
        // 3-gram: "hel", "ell", "llo" (3つ)
        // 4-gram: "hell", "ello" (2つ)
        expect(ngrams).toContain("he");
        expect(ngrams).toContain("el");
        expect(ngrams).toContain("ll");
        expect(ngrams).toContain("lo");
        expect(ngrams).toContain("hel");
        expect(ngrams).toContain("ell");
        expect(ngrams).toContain("llo");
        expect(ngrams).toContain("hell");
        expect(ngrams).toContain("ello");
      });

      it("日本語テキストからN-gramを生成できる", () => {
        const ngrams = generateNgrams("こんにちは");

        // "こんにちは" (5文字)
        // 2-gram: "こん", "んに", "にち", "ちは"
        // 3-gram: "こんに", "んにち", "にちは"
        // 4-gram: "こんにち", "んにちは"
        expect(ngrams).toContain("こん");
        expect(ngrams).toContain("んに");
        expect(ngrams).toContain("にち");
        expect(ngrams).toContain("ちは");
      });

      it("デフォルトでは2-gramから4-gramを生成する", () => {
        const ngrams = generateNgrams("abcde");

        // 2-gram: ab, bc, cd, de (4つ)
        // 3-gram: abc, bcd, cde (3つ)
        // 4-gram: abcd, bcde (2つ)
        // 合計: 9つのユニークなN-gram
        expect(ngrams.length).toBe(9);
      });
    });

    describe("テキストの正規化", () => {
      it("大文字が小文字に変換される", () => {
        const ngrams = generateNgrams("HELLO");

        expect(ngrams).toContain("he");
        expect(ngrams).toContain("hel");
        expect(ngrams).not.toContain("HE");
      });

      it("空白が除去される", () => {
        const ngrams = generateNgrams("hello world");

        // "hello world" → "helloworld"
        expect(ngrams).toContain("ow");
        expect(ngrams).toContain("wor");
      });

      it("記号が除去される", () => {
        const ngrams = generateNgrams("hello, world!");

        // "hello, world!" → "helloworld"
        expect(ngrams).toContain("ow");
        expect(ngrams.every((ngram) => !ngram.includes(","))).toBe(true);
        expect(ngrams.every((ngram) => !ngram.includes("!"))).toBe(true);
      });

      it("複数の空白が除去される", () => {
        const ngrams = generateNgrams("hello   world");

        // 正規化後は "helloworld"
        expect(ngrams).toContain("ow");
      });
    });

    describe("カスタムN-gramサイズ", () => {
      it("最小値を指定してN-gramを生成できる", () => {
        const ngrams = generateNgrams("hello", 3, 4);

        // 3-gram: "hel", "ell", "llo" (3つ)
        // 4-gram: "hell", "ello" (2つ)
        expect(ngrams.length).toBe(5);
        expect(ngrams.every((ngram) => ngram.length >= 3)).toBe(true);
      });

      it("最大値を指定してN-gramを生成できる", () => {
        const ngrams = generateNgrams("hello", 2, 3);

        // 2-gram: 4つ
        // 3-gram: 3つ
        expect(ngrams.length).toBe(7);
        expect(ngrams.every((ngram) => ngram.length <= 3)).toBe(true);
      });

      it("同じ最小値と最大値で単一サイズのN-gramを生成できる", () => {
        const ngrams = generateNgrams("hello", 2, 2);

        // 2-gramのみ
        expect(ngrams.every((ngram) => ngram.length === 2)).toBe(true);
        expect(ngrams.length).toBe(4);
      });
    });

    describe("エッジケース", () => {
      it("空の文字列からは空の配列を返す", () => {
        const ngrams = generateNgrams("");

        expect(ngrams).toEqual([]);
      });

      it("短いテキストでは生成可能なN-gramのみ返す", () => {
        const ngrams = generateNgrams("ab");

        // "ab" (2文字)
        // 2-gram: "ab" (1つ)
        // 3-gram, 4-gram: なし
        expect(ngrams.length).toBe(1);
        expect(ngrams).toContain("ab");
      });

      it("最小サイズより短いテキストでは空の配列を返す", () => {
        const ngrams = generateNgrams("a", 2, 4);

        expect(ngrams).toEqual([]);
      });

      it("重複するN-gramは除去される", () => {
        const ngrams = generateNgrams("aaa");

        // "aaa" → 正規化後 "aaa"
        // 2-gram: "aa" (重複なので1つ)
        // 3-gram: "aaa" (1つ)
        expect(ngrams).toContain("aa");
        expect(ngrams).toContain("aaa");
        expect(ngrams.length).toBe(2);
      });

      it("空白のみの文字列からは空の配列を返す", () => {
        const ngrams = generateNgrams("   ");

        expect(ngrams).toEqual([]);
      });

      it("記号のみの文字列からは空の配列を返す", () => {
        const ngrams = generateNgrams("!@#$%");

        expect(ngrams).toEqual([]);
      });
    });
  });

  describe("score", () => {
    describe("基本的なスコアリング", () => {
      it("ボーナス関数に基づいてスコアを計算する", () => {
        const bonus = (ngram: string): number => {
          return ngram.length;
        };

        const scorer = score(bonus);
        const ngrams = ["ab", "abc", "abcd"];
        const result = scorer(ngrams, "test", "field");

        // 2 + 3 + 4 = 9
        expect(result).toBe(9);
      });

      it("空のN-gram配列では0を返す", () => {
        const bonus = (): number => 1;

        const scorer = score(bonus);
        const result = scorer([], "test", "field");

        expect(result).toBe(0);
      });
    });

    describe("コンテキストの利用", () => {
      it("targetSetがコンテキストに含まれる", () => {
        const contexts: Array<{
          targetSet: Set<string>;
          field: string;
        }> = [];

        const bonus = (
          ngram: string,
          context: { targetSet: Set<string>; field: string }
        ): number => {
          contexts.push(context);
          return 1;
        };

        const scorer = score(bonus);
        scorer(["ab"], "test", "field");

        expect(contexts.length).toBe(1);
        expect(contexts[0].targetSet).toBeDefined();
        expect(contexts[0].targetSet.has("t")).toBe(true);
        expect(contexts[0].targetSet.has("e")).toBe(true);
        expect(contexts[0].targetSet.has("s")).toBe(true);
      });

      it("fieldがコンテキストに含まれる", () => {
        let capturedField: string = "";

        const bonus = (
          _ngram: string,
          context: { targetSet: Set<string>; field: string }
        ): number => {
          capturedField = context.field;
          return 1;
        };

        const scorer = score(bonus);
        scorer(["ab"], "test", "myField");

        expect(capturedField).toBe("myField");
      });
    });

    describe("スコアリングの例", () => {
      it("文字が含まれる場合にボーナスを与えるスコアラー", () => {
        const bonus = (
          ngram: string,
          context: { targetSet: Set<string>; field: string }
        ): number => {
          let matches = 0;
          for (const char of ngram) {
            if (context.targetSet.has(char)) {
              matches++;
            }
          }
          return matches;
        };

        const scorer = score(bonus);
        // "ab" contains 'a' which is in "abc"
        // "bc" contains 'b' and 'c' which are in "abc"
        const result = scorer(["ab", "bc"], "abc", "test");

        // "ab": 'a' in "abc" = 1, 'b' in "abc" = 1 → 2
        // "bc": 'b' in "abc" = 1, 'c' in "abc" = 1 → 2
        // total = 4
        expect(result).toBe(4);
      });
    });

    describe("型パラメータ", () => {
      it("異なるフィールド型でスコアリングできる", () => {
        type FieldType = { weight: number };

        const bonus = (
          _ngram: string,
          context: { targetSet: Set<string>; field: FieldType }
        ): number => {
          return context.field.weight;
        };

        const scorer: Scorer<FieldType> = score(bonus);
        const result = scorer(["ab", "cd"], "test", { weight: 5 });

        // 5 + 5 = 10
        expect(result).toBe(10);
      });
    });
  });
});
