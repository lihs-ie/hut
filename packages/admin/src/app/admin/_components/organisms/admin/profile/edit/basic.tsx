"use client";

import { SimpleCard } from "@shared/components/atoms/card/simple";
import { TextInput } from "@shared/components/atoms/input/text";
import { Textarea } from "@shared/components/atoms/input/textarea";
import { FormField } from "@shared/components/molecules/form/field";
import { FormFieldRow } from "@shared/components/molecules/form/field-row";
import styles from "./basic.module.css";

export type BasicInfo = {
  name: string;
  bio: string;
  githubUsername: string;
  twitterUsername: string;
};

export type Value = {
  name: string;
  bio: string;
  externalServices: {
    service: string;
    user: string;
  }[];
};

export type Props = {
  value: Value;
  onUpdate: (value: Value) => void;
};

export const ProfileBasicInfoForm = (props: Props) => (
  <SimpleCard className={styles.container}>
    <FormField label="表示名" required htmlFor="name">
      <TextInput
        value={props.value.name}
        onChange={(value) => props.onUpdate({ ...props.value, name: value })}
      />
    </FormField>

    <FormField label="自己紹介" htmlFor="bio">
      <Textarea
        value={props.value.bio}
        onChange={(value) => props.onUpdate({ ...props.value, bio: value })}
        placeholder="自己紹介を入力"
        rows={4}
      />
    </FormField>

    <FormFieldRow>
      <FormField label="GitHubユーザーID" htmlFor="github">
        <TextInput
          value={
            props.value.externalServices.find(
              (service) => service.service === "gitHub",
            )?.user || ""
          }
          onChange={(value) => {
            const existingService = props.value.externalServices.find(
              (service) => service.service === "gitHub",
            );
            const updatedServices = existingService
              ? props.value.externalServices.map((service) =>
                  service.service === "gitHub"
                    ? { ...service, user: value }
                    : service,
                )
              : [
                  ...props.value.externalServices,
                  { service: "gitHub", user: value },
                ];
            props.onUpdate({
              ...props.value,
              externalServices: updatedServices,
            });
          }}
          placeholder="ユーザーIDだけを入力"
        />
      </FormField>

      <FormField label="XユーザーID" htmlFor="x">
        <TextInput
          value={
            props.value.externalServices.find(
              (service) => service.service === "x",
            )?.user || ""
          }
          onChange={(value) => {
            const existingService = props.value.externalServices.find(
              (service) => service.service === "x",
            );
            const updatedServices = existingService
              ? props.value.externalServices.map((service) =>
                  service.service === "x"
                    ? { ...service, user: value }
                    : service,
                )
              : [
                  ...props.value.externalServices,
                  { service: "x", user: value },
                ];
            props.onUpdate({
              ...props.value,
              externalServices: updatedServices,
            });
          }}
          placeholder="ユーザーIDだけを入力"
        />
      </FormField>
    </FormFieldRow>

    {/* TODO: 後々追加する
      <FormField label="その他のウェブサイト" htmlFor="website">
        <TextInput
          value={props.value.externalServices.find(service => service.service === "website")?.user || ""}
          onChange={handleWebsiteChange}
          placeholder="https://example.com"
        />
      </FormField> */}

    <p className={styles.note}>プロフィールにこれらの情報が表示されます。</p>
  </SimpleCard>
);
