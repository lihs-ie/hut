import { describe, it, expect } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  createSeriesCreatedEvent,
  createSeriesUpdatedEvent,
  createSeriesTerminatedEvent,
} from "@shared/domains/series/event";
import { SeriesIdentifierMold } from "../../support/molds/domains/series";

describe("domains/series/event", () => {
  describe("createSeriesCreatedEvent", () => {
    it("typeがseries.persistedのイベントを返す", () => {
      const seriesIdentifier = Forger(SeriesIdentifierMold).forgeWithSeed(1);

      const event = createSeriesCreatedEvent(seriesIdentifier);

      expect(event.type).toBe("series.persisted");
    });

    it("payloadのseriesに識別子が設定される", () => {
      const seriesIdentifier = Forger(SeriesIdentifierMold).forgeWithSeed(2);

      const event = createSeriesCreatedEvent(seriesIdentifier);

      expect(event.payload.series).toBe(seriesIdentifier);
    });

    it("occurredAtが設定される", () => {
      const seriesIdentifier = Forger(SeriesIdentifierMold).forgeWithSeed(3);
      const before = new Date();

      const event = createSeriesCreatedEvent(seriesIdentifier);

      const after = new Date();
      expect(event.occurredAt.getTime()).toBeGreaterThanOrEqual(before.getTime());
      expect(event.occurredAt.getTime()).toBeLessThanOrEqual(after.getTime());
    });

    it("identifierがULID形式で生成される", () => {
      const seriesIdentifier = Forger(SeriesIdentifierMold).forgeWithSeed(4);

      const event = createSeriesCreatedEvent(seriesIdentifier);

      expect(event.identifier).toMatch(/^[0-9A-Z]{26}$/);
    });

    it("呼び出しごとに異なるidentifierが生成される", () => {
      const seriesIdentifier = Forger(SeriesIdentifierMold).forgeWithSeed(5);

      const event1 = createSeriesCreatedEvent(seriesIdentifier);
      const event2 = createSeriesCreatedEvent(seriesIdentifier);

      expect(event1.identifier).not.toBe(event2.identifier);
    });
  });

  describe("createSeriesUpdatedEvent", () => {
    it("typeがseries.persistedのイベントを返す", () => {
      const seriesIdentifier = Forger(SeriesIdentifierMold).forgeWithSeed(10);

      const event = createSeriesUpdatedEvent(seriesIdentifier);

      expect(event.type).toBe("series.persisted");
    });

    it("payloadのseriesに識別子が設定される", () => {
      const seriesIdentifier = Forger(SeriesIdentifierMold).forgeWithSeed(11);

      const event = createSeriesUpdatedEvent(seriesIdentifier);

      expect(event.payload.series).toBe(seriesIdentifier);
    });

    it("occurredAtが設定される", () => {
      const seriesIdentifier = Forger(SeriesIdentifierMold).forgeWithSeed(12);
      const before = new Date();

      const event = createSeriesUpdatedEvent(seriesIdentifier);

      const after = new Date();
      expect(event.occurredAt.getTime()).toBeGreaterThanOrEqual(before.getTime());
      expect(event.occurredAt.getTime()).toBeLessThanOrEqual(after.getTime());
    });

    it("identifierがULID形式で生成される", () => {
      const seriesIdentifier = Forger(SeriesIdentifierMold).forgeWithSeed(13);

      const event = createSeriesUpdatedEvent(seriesIdentifier);

      expect(event.identifier).toMatch(/^[0-9A-Z]{26}$/);
    });

    it("呼び出しごとに異なるidentifierが生成される", () => {
      const seriesIdentifier = Forger(SeriesIdentifierMold).forgeWithSeed(14);

      const event1 = createSeriesUpdatedEvent(seriesIdentifier);
      const event2 = createSeriesUpdatedEvent(seriesIdentifier);

      expect(event1.identifier).not.toBe(event2.identifier);
    });
  });

  describe("createSeriesTerminatedEvent", () => {
    it("typeがseries.terminatedのイベントを返す", () => {
      const seriesIdentifier = Forger(SeriesIdentifierMold).forgeWithSeed(20);

      const event = createSeriesTerminatedEvent(seriesIdentifier);

      expect(event.type).toBe("series.terminated");
    });

    it("payloadのseriesに識別子が設定される", () => {
      const seriesIdentifier = Forger(SeriesIdentifierMold).forgeWithSeed(21);

      const event = createSeriesTerminatedEvent(seriesIdentifier);

      expect(event.payload.series).toBe(seriesIdentifier);
    });

    it("occurredAtが設定される", () => {
      const seriesIdentifier = Forger(SeriesIdentifierMold).forgeWithSeed(22);
      const before = new Date();

      const event = createSeriesTerminatedEvent(seriesIdentifier);

      const after = new Date();
      expect(event.occurredAt.getTime()).toBeGreaterThanOrEqual(before.getTime());
      expect(event.occurredAt.getTime()).toBeLessThanOrEqual(after.getTime());
    });

    it("identifierがULID形式で生成される", () => {
      const seriesIdentifier = Forger(SeriesIdentifierMold).forgeWithSeed(23);

      const event = createSeriesTerminatedEvent(seriesIdentifier);

      expect(event.identifier).toMatch(/^[0-9A-Z]{26}$/);
    });

    it("呼び出しごとに異なるidentifierが生成される", () => {
      const seriesIdentifier = Forger(SeriesIdentifierMold).forgeWithSeed(24);

      const event1 = createSeriesTerminatedEvent(seriesIdentifier);
      const event2 = createSeriesTerminatedEvent(seriesIdentifier);

      expect(event1.identifier).not.toBe(event2.identifier);
    });
  });
});
