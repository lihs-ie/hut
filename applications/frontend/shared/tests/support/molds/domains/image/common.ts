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
import { ArticleIdentifier } from "@shared/domains/articles";
import { MemoIdentifier } from "@shared/domains/memo";

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
  value: ArticleIdentifier | MemoIdentifier;
  contentType: "article" | "memo";
};

export const ImageReferenceMold = Mold<
  ArticleIdentifier | MemoIdentifier,
  ImageReferenceProperties
>({
  pour: (properties) => properties.value,
  prepare: (overrides, seed) => {
    const contentType =
      overrides.contentType ?? (seed % 2 === 0 ? "article" : "memo");
    if (contentType === "article") {
      return {
        value:
          overrides.value ?? Forger(ArticleIdentifierMold).forgeWithSeed(seed),
        contentType: "article",
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
  reference: ArticleIdentifier | MemoIdentifier;
  content: "article" | "memo";
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
      overrides.content ?? (seed % 2 === 0 ? "article" : "memo");
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
  reference?: ArticleIdentifier | MemoIdentifier;
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
