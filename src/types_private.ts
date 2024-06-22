export type Optional<T> = T | undefined;
export type Nullable<T> = T | null;
export type Pure<T> = Exclude<T, undefined | null>;
export type MaybeArray<T> = T | T[];
