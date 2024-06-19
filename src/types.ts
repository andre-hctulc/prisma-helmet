export type InFilter<T = string> = { in: T[] };
export type ContainsFilter<T = string> = { contains: T };
export type Or<T> = { OR: T[] };
export type And<T> = { AND: T };
export type FieldFilter<T = string> = T | InFilter<T>;

/**
 * `Prisma.<P>GetPayload<{ select: <S> }>`
 *
 * @example
 * ```ts
 * Prisma.UserGetPayload<{ select: typeof userSelect }
 * ```
 *
 * Note that this type is only for demonstration purposes and must be implemented manually,
 * as Prisma generates these types dynamically.
 *  */
export type Mask<P, S> = never;
