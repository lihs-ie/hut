import z from "zod";

export const sortByFieldSchema = z
  .enum(["createdAt", "updatedAt"])
  .brand("SortByField");

export type SortByField = z.infer<typeof sortByFieldSchema>;

export const SortByField = {
  CREATED_AT: sortByFieldSchema.parse("createdAt"),
  UPDATED_AT: sortByFieldSchema.parse("updatedAt"),
} as const;

export const orderSchema = z.enum(["asc", "desc"]).brand("Order");

export type Order = z.infer<typeof orderSchema>;

export const Order = {
  ASC: orderSchema.parse("asc"),
  DESC: orderSchema.parse("desc"),
} as const;
