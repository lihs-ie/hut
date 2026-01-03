import z from "zod";

export const timelineSchema = z
  .object({
    createdAt: z.date(),
    updatedAt: z.date(),
  })
  .refine(
    (data) => {
      return data.updatedAt >= data.createdAt;
    },
    {
      message: "updatedAt must be greater than or equal to createdAt",
    }
  );

export type Timeline = z.infer<typeof timelineSchema>;
