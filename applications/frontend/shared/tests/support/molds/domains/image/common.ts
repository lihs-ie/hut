import {
  Image,
  ImageIdentifier,
  imageIdentifierSchema,
  ImageType,
  ImageURL,
  imageURLSchema,
  UploadStatus,
  imageSchema,
  Criteria,
  criteriaSchema,
} from "@shared/domains/image";
import { ulid } from "ulid";
import { EnumMold, Forger, Mold } from "@lihs-ie/forger-ts";
import { DateMold } from "../common/date";
import { ArticleIdentifierMold } from "../article/common";
import { MemoIdentifierMold } from "../memo/common";
import { SeriesIdentifierMold } from "../series/common";
import { ArticleIdentifier } from "@shared/domains/articles";
import { MemoIdentifier } from "@shared/domains/memo";
import { SeriesIdentifier } from "@shared/domains/series";

export const ImageTypeMold = EnumMold(ImageType);

export const UploadStatusMold = EnumMold(UploadStatus);

export type ImageIdentifierProperties = {
  value: string;
};

export const ImageIdentifierMold = Mold<
  ImageIdentifier,
  ImageIdentifierProperties
>({
  pour: (properties) => imageIdentifierSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value:
      overrides.value ?? ulid(Forger(DateMold).forgeWithSeed(seed).getTime()),
  }),
});

export type ImageURLProperties = {
  value: string;
};

export const ImageURLMold = Mold<ImageURL, ImageURLProperties>({
  pour: (properties) => imageURLSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value: overrides.value ?? `https://example.com/images/${seed}.png`,
  }),
});

export type ImageReferenceProperties = {
  value: ArticleIdentifier | MemoIdentifier | SeriesIdentifier;
  contentType: "article" | "memo" | "series";
};

export const ImageReferenceMold = Mold<
  ArticleIdentifier | MemoIdentifier | SeriesIdentifier,
  ImageReferenceProperties
>({
  pour: (properties) => properties.value,
  prepare: (overrides, seed) => {
    const contentType =
      overrides.contentType ??
      (["article", "memo", "series"] as const)[seed % 3];
    if (contentType === "article") {
      return {
        value:
          overrides.value ?? Forger(ArticleIdentifierMold).forgeWithSeed(seed),
        contentType: "article",
      };
    }
    if (contentType === "series") {
      return {
        value:
          overrides.value ?? Forger(SeriesIdentifierMold).forgeWithSeed(seed),
        contentType: "series",
      };
    }
    return {
      value: overrides.value ?? Forger(MemoIdentifierMold).forgeWithSeed(seed),
      contentType: "memo",
    };
  },
});

export type ImageProperties = {
  identifier: ImageIdentifier;
  type: ImageType;
  url: ImageURL | null;
  uploadStatus: UploadStatus;
  reference: ArticleIdentifier | MemoIdentifier | SeriesIdentifier;
  content: "article" | "memo" | "series";
};

export const ImageMold = Mold<Image, ImageProperties>({
  pour: (properties) =>
    imageSchema.parse({
      identifier: properties.identifier,
      type: properties.type,
      url: properties.url,
      uploadStatus: properties.uploadStatus,
      reference: properties.reference,
      content: properties.content,
    }),
  prepare: (overrides, seed) => {
    const uploadStatus =
      overrides.uploadStatus ?? Forger(UploadStatusMold).forgeWithSeed(seed);
    const url =
      uploadStatus === UploadStatus.COMPLETED
        ? (overrides.url ?? Forger(ImageURLMold).forgeWithSeed(seed))
        : null;
    const contentType =
      overrides.content ??
      (["article", "memo", "series"] as const)[seed % 3];
    const reference =
      overrides.reference ??
      Forger(ImageReferenceMold).forgeWithSeed(seed, { contentType });

    return {
      identifier:
        overrides.identifier ?? Forger(ImageIdentifierMold).forgeWithSeed(seed),
      type: overrides.type ?? Forger(ImageTypeMold).forgeWithSeed(seed),
      url,
      uploadStatus,
      reference,
      content: contentType,
    };
  },
});

export type CriteriaProperties = {
  type?: ImageType;
  uploadStatus?: UploadStatus;
  reference?: ArticleIdentifier | MemoIdentifier | SeriesIdentifier;
};

export const ImageCriteriaMold = Mold<Criteria, CriteriaProperties>({
  pour: (properties) =>
    criteriaSchema.parse({
      type: properties.type,
      uploadStatus: properties.uploadStatus,
      reference: properties.reference,
    }),
  prepare: (overrides) => ({
    type: overrides.type,
    uploadStatus: overrides.uploadStatus,
    reference: overrides.reference,
  }),
});
