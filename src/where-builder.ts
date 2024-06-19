import type { Prisma } from "@prisma/client";
import { Nullable, Optional, OrArrayOf } from "./types_private";

type BuilderInput<T> = Optional<Nullable<T>>;
type WhereField<W> = keyof W;
// TypeScripts Extract uses never instead of undefined, but never extends every type
type Extr<T, U> = T extends U ? T : null;
/** Extracts the field value from a where input */
type FieldValue<W, F extends WhereField<W>> = Extr<W[F], Date> extends Date
    ? Date
    : Extr<W[F], Buffer> extends Buffer
    ? Buffer
    : Extr<W[F], number> extends number
    ? number
    : Extr<W[F], boolean> extends boolean
    ? boolean
    : Extr<W[F], string> extends string
    ? string
    : any;

type WhereValidator<W> = (where: W) => boolean;

type test = Prisma.BucketItemWhereInput;

type F = FieldValue<test, "id">;
type E = Extract<test["id"], boolean>;

/* 
Settings a filter (contains, in, ...) shoudl always set the targeted field in the filter itself
to ensure consistent behavior. 
*/

export class WhereBuilder<W> {
    static from = <W>(where: W) => new WhereBuilder(where);
    static create = <W>() => new WhereBuilder<W>();

    private where: any = {};
    private validators = new Set<WhereValidator<W>>();

    constructor(where?: W) {
        if (where) this.where = where;
    }

    build(): W {
        for (const val of this.validators) {
            if (!val(this.where)) throw new Error("Validation failed at where clause (WhereBuilder.build)");
        }
        return this.where;
    }

    addValidator(validate: WhereValidator<W>) {
        this.validators.add(validate);
        return this;
    }

    removeValidator(validate: WhereValidator<W>) {
        this.validators.delete(validate);
        return this;
    }

    and<K extends string, V>(...where: W[]) {
        if (!this.where.AND) this.where.AND = [];
        this.where.AND.push(...where);
        return this;
    }

    or<K extends string, V>(...where: W[]) {
        if (!this.where.OR) this.where.OR = [];
        this.where.OR.push(...where);
        return this;
    }

    contribute(where: W) {
        for (const key in where) {
            this.where[key] = where[key];
        }
        return this;
    }

    in<F extends WhereField<W>>(field: F, value: BuilderInput<OrArrayOf<FieldValue<W, F>>>) {
        if (Array.isArray(value)) {
            this.where[field] = { in: value };
        } else {
            this.where[field] = value;
        }
        return this;
    }

    contains<F extends WhereField<W>>(field: F, value: BuilderInput<OrArrayOf<FieldValue<W, F>>>) {
        if (value == null) {
            this.where[field] = value;
        } else if (Array.isArray(value)) {
            this.and({ OR: value.map((v) => this.contains(field, v)) } as W);
            this.where[field] = undefined;
        } else {
            this.where[field] = { contains: value };
        }
        return this;
    }
}
