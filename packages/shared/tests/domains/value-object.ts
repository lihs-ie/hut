import { Properties, ValueObject } from "@shared/domains/common/value-object";

export const ValueObjectTest = <
  T extends Record<string, unknown>,
  P extends Properties<T>
>(
  factory: (properties: P) => T,
  defaultProperty: P,
  variations: unknown[] = [],
  invalids: unknown[] = []
) => {
  describe("Package value-object", () => {
    describe("ValueObject", () => {
      describe("instantiate", () => {
        describe("successfully", () => {
          it.each([defaultProperty, ...variations])(
            "should returns a value object %s",
            (variation) => {
              const object = factory(
                Object.assign({}, defaultProperty, variation)
              );

              expect(object).toBeDefined();
              expect(object).toHaveProperty("equals");
              expect(object).toHaveProperty("hashCode");
              expect(typeof object.equals).toBe("function");
              expect(typeof object.hashCode).toBe("function");
            }
          );
        });

        describe("unsuccessfully", () => {
          it.each(invalids)("should throws error %s", (invalid) => {
            expect(() =>
              factory(Object.assign({}, defaultProperty, invalid as P))
            ).toThrowError();
          });
        });
      });

      describe("hashCode", () => {
        it.each(variations)(
          "should returns same hash code for same properties %s",
          (variation) => {
            const property = Object.assign({}, defaultProperty, variation);

            const object1 = factory(property);
            const object2 = factory(property);

            const actual1 = object1.hashCode();
            const actual2 = object2.hashCode();

            expect(actual1).toBe(actual2);
          }
        );

        it.each(variations)(
          "should returns different hash code for different properties %s",
          (variation) => {
            const object1 = factory(defaultProperty);
            const object2 = factory(
              Object.assign({}, defaultProperty, variation)
            );

            const actual1 = object1.hashCode();
            const actual2 = object2.hashCode();

            expect(actual1).not.toBe(actual2);
          }
        );
      });

      describe("equals", () => {
        it.each([defaultProperty, ...variations])(
          "should returns true for same properties %s",
          (variation) => {
            const property = Object.assign({}, defaultProperty, variation);

            const object1 = factory(property);
            const object2 = factory(property);

            const actual = object1.equals(object2);

            expect(actual).toBeTruthy();
          }
        );

        it.each(variations)(
          "should returns false for different properties %s",
          (variation) => {
            const object1 = factory(defaultProperty);
            const object2 = factory(
              Object.assign({}, defaultProperty, variation)
            );

            const actual = object1.equals(object2);

            expect(actual).toBeFalsy();
          }
        );
      });
    });
  });
};
