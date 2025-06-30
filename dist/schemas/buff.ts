import * as z from "zod";


export const BuffElement = z.object({
    "attributes": z.array(z.string()).optional(),
    "classes": z.array(z.string()).optional(),
    "conditions": z.array(z.string()).optional(),
});
export type BuffElement = z.infer<typeof BuffElement>;
