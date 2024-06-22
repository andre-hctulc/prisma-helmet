import type { Prisma } from "@prisma/client";
import type { Nullable, Optional, MaybeArray } from "./types_private";

type BuilderInput<T> = Optional<Nullable<T>>;
type Field<W> = keyof W;
type WhereValidator<W> = (where: Partial<W>) => boolean;
/** Field Type */
type FT<W, F extends Field<W>> = string extends W[F]
    ? string
    : boolean extends W[F]
    ? boolean
    : number extends W[F]
    ? number
    : Date extends W[F]
    ? Date
    : any;

/* 
Settings a filter (contains, in, ...) should always set the targeted field in the filter itself
to ensure consistent behavior. 
*/

export class WhereBuilder<W> {
    static from = <W>(where: Partial<W>) => new WhereBuilder(where);
    static create = <W>() => new WhereBuilder<W>();

    private _where: any = {};
    private _validators = new Set<WhereValidator<W>>();

    constructor(base: Partial<W> = {}) {
        if (base) this._where = base;
    }

    build(): W {
        for (const val of this._validators) {
            if (!val(this._where)) throw new Error("Validation failed at where clause (WhereBuilder.build)");
        }
        return this._where;
    }

    addValidator(validate: WhereValidator<W>) {
        this._validators.add(validate);
        return this;
    }

    removeValidator(validate: WhereValidator<W>) {
        this._validators.delete(validate);
        return this;
    }

    and(...where: Partial<W>[]) {
        if (!this._where.AND) this._where.AND = [];
        this._where.AND.push(...where);
        return this;
    }

    or<V>(...where: Partial<W>[]) {
        if (!this._where.OR) this._where.OR = [];
        this._where.OR.push(...where);
        return this;
    }

    contribute(where: Partial<W>) {
        for (const key in where) {
            this._where[key] = where[key];
        }
        return this;
    }

    in<F extends Field<W>>(field: F, value: BuilderInput<MaybeArray<FT<W, F>>>) {
        if (Array.isArray(value)) {
            this._where[field] = { in: value };
        } else {
            this._where[field] = value;
        }
        return this;
    }

    contains<F extends Field<W>>(field: F, value: BuilderInput<MaybeArray<FT<W, F>>>) {
        if (value == null) {
            this._where[field] = value;
        } else if (Array.isArray(value)) {
            this.and({ OR: value.map((v) => this.contains(field, v)) } as W);
            this._where[field] = undefined;
        } else {
            this._where[field] = { contains: value };
        }
        return this;
    }

    equals<F extends Field<W>>(field: F, value: BuilderInput<FT<W, F>>) {
        this._where[field] = value;
        return this;
    }

    private _jsonPath(path: string[]) {
        // TODO  postgresql expects an array of strings here
        return `$.${path.join(".")}`;
    }

    jsonField<F extends Field<W>>(field: F, path: string[], value: BuilderInput<any>) {
        const filter: Prisma.JsonFilter = { path: this._jsonPath(path), equals: value };
        this._where[field] = filter;
        return this;
    }

    jsonFields<F extends Field<W>>(field: F, obj: BuilderInput<Record<string, any>>) {
        if (!obj) return this;
        this.and(
            ...Object.keys(obj).map(
                (key) => ({ [field]: { path: this._jsonPath([key]), equals: obj[key] } } as W)
            )
        );
        return this;
    }

    jsonArrayContains<F extends Field<W>>(field: F, path: string[], includes: any) {
        const filter: Prisma.JsonFilter = { path: this._jsonPath(path), array_contains: includes };
        this._where[field] = filter;
        return this;
    }
}
