// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

static ERL_NIF_TERM
h2o_nif_string_tolower_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1) {
        return enif_make_badarg(env);
    }
    int ch;
    if (!enif_get_int(env, argv[0], &ch)) {
        return enif_make_badarg(env);
    }
    return enif_make_int(env, h2o_tolower(ch));
}

static size_t
h2o_nif_string_strtolower_1_map(ErlNifEnv *env, h2o_nif_slice_t *slice, size_t offset, size_t length)
{
    (void)h2o_strtolower((char *)(slice->out.data + offset), length);
    return (offset + length);
}

static ERL_NIF_TERM
h2o_nif_string_strtolower_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;

    if (argc != 1 || !enif_inspect_iolist_as_binary(env, argv[0], &in)) {
        return enif_make_badarg(env);
    }

    if (in.size <= MAX_PER_SLICE) {
        ERL_NIF_TERM out;
        unsigned char *buf = enif_make_new_binary(env, in.size, &out);
        (void)memcpy(buf, in.data, in.size);
        (void)h2o_strtolower((char *)buf, in.size);
        return out;
    }

    h2o_nif_slice_t *slice =
        h2o_nif_slice_create(env, "string_strtolower", in.size, 0, h2o_nif_string_strtolower_1_map, NULL, NULL);
    if (!enif_alloc_binary(in.size, &slice->out)) {
        (void)h2o_nif_slice_release(slice);
        return enif_make_badarg(env);
    }
    (void)memcpy(slice->out.data, in.data, in.size);

    return h2o_nif_slice_schedule(env, slice);
}

static ERL_NIF_TERM
h2o_nif_string_toupper_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1) {
        return enif_make_badarg(env);
    }
    int ch;
    if (!enif_get_int(env, argv[0], &ch)) {
        return enif_make_badarg(env);
    }
    return enif_make_int(env, h2o_toupper(ch));
}

static size_t
h2o_nif_string_strtoupper_1_map(ErlNifEnv *env, h2o_nif_slice_t *slice, size_t offset, size_t length)
{
    (void)h2o_strtoupper((char *)(slice->out.data + offset), length);
    return (offset + length);
}

static ERL_NIF_TERM
h2o_nif_string_strtoupper_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;

    if (argc != 1 || !enif_inspect_iolist_as_binary(env, argv[0], &in)) {
        return enif_make_badarg(env);
    }

    if (in.size <= MAX_PER_SLICE) {
        ERL_NIF_TERM out;
        unsigned char *buf = enif_make_new_binary(env, in.size, &out);
        (void)memcpy(buf, in.data, in.size);
        (void)h2o_strtoupper((char *)buf, in.size);
        return out;
    }

    h2o_nif_slice_t *slice =
        h2o_nif_slice_create(env, "string_strtoupper", in.size, 0, h2o_nif_string_strtoupper_1_map, NULL, NULL);
    if (!enif_alloc_binary(in.size, &slice->out)) {
        (void)h2o_nif_slice_release(slice);
        return enif_make_badarg(env);
    }
    (void)memcpy(slice->out.data, in.data, in.size);

    return h2o_nif_slice_schedule(env, slice);
}

static size_t
h2o_nif_string_lcstris_2_map(ErlNifEnv *env, h2o_nif_slice_t *slice, size_t offset, size_t length)
{
    int result = slice->flags;
    if (result == 0) {
        return slice->length;
    }
    result = h2o_lcstris((const char *)slice->in.data, slice->in.size, (const char *)slice->out.data, slice->out.size);
    if (result) {
        return (offset + length);
    } else {
        slice->flags = result;
        return slice->length;
    }
}

static ERL_NIF_TERM
h2o_nif_string_lcstris_2_reduce(ErlNifEnv *env, h2o_nif_slice_t *slice)
{
    int result = slice->flags;
    ERL_NIF_TERM out;
    if (result) {
        out = enif_make_atom(env, "true");
    } else {
        out = enif_make_atom(env, "false");
    }
    return out;
}

static ERL_NIF_TERM
h2o_nif_string_lcstris_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary target;
    ErlNifBinary test;

    if (argc != 2 || !enif_inspect_iolist_as_binary(env, argv[0], &target) || !enif_inspect_iolist_as_binary(env, argv[1], &test)) {
        return enif_make_badarg(env);
    }

    if (target.size <= MAX_PER_SLICE || target.size != test.size) {
        ERL_NIF_TERM out;
        if (h2o_lcstris((const char *)target.data, target.size, (const char *)test.data, test.size)) {
            out = enif_make_atom(env, "true");
        } else {
            out = enif_make_atom(env, "false");
        }
        return out;
    }

    h2o_nif_slice_t *slice = h2o_nif_slice_create(env, "string_lcstris", target.size, 0, h2o_nif_string_lcstris_2_map,
                                                  h2o_nif_string_lcstris_2_reduce, NULL);
    slice->flags = 1;
    slice->in = target;
    slice->out = test;

    return h2o_nif_slice_schedule(env, slice);
}

static ERL_NIF_TERM
h2o_nif_string_base64_encode_capacity_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned int len;

    if (argc != 1 || !enif_get_uint(env, argv[0], &len)) {
        return enif_make_badarg(env);
    }

    return enif_make_uint(env, h2o_base64_encode_capacity(len));
}

static size_t
h2o_nif_string_decode_base64url_1_map(ErlNifEnv *env, h2o_nif_slice_t *slice, size_t offset, size_t length)
{
    size_t offset2 = slice->offset2;

    h2o_iovec_t outv = h2o_decode_base64url(slice->pool, (const char *)(slice->in.data + offset), length);
    (void)memcpy(slice->out.data + offset2, outv.base, outv.len);

    slice->offset2 += outv.len;

    (void)h2o_mem_clear_pool(slice->pool);

    return (offset + length);
}

static ERL_NIF_TERM
h2o_nif_string_decode_base64url_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;
    ErlNifBinary outbin;
    size_t outlen;

    if (argc != 1 || !enif_inspect_iolist_as_binary(env, argv[0], &in)) {
        return enif_make_badarg(env);
    }

    if (in.size <= MAX_PER_SLICE) {
        ERL_NIF_TERM out;
        h2o_iovec_t outv = h2o_decode_base64url(NULL, (const char *)in.data, in.size);
        if (outv.base == NULL) {
            return enif_make_badarg(env);
        }
        unsigned char *buf = enif_make_new_binary(env, outv.len, &out);
        (void)memcpy(buf, outv.base, outv.len);
        (void)free(outv.base);
        return out;
    }

    outlen = ((in.size * 3) / 4);

    if (!enif_alloc_binary(outlen, &outbin)) {
        return enif_make_badarg(env);
    }
    h2o_nif_slice_t *slice =
        h2o_nif_slice_create(env, "string_base64_encode", in.size, 0, h2o_nif_string_decode_base64url_1_map, NULL, NULL);
    slice->offset2 = 0;
    slice->in = in;
    slice->out = outbin;
    slice->pool = (h2o_mem_pool_t *)enif_alloc(sizeof(h2o_mem_pool_t));
    (void)memset(slice->pool, 0, sizeof(h2o_mem_pool_t));
    (void)h2o_mem_init_pool(slice->pool);

    return h2o_nif_slice_schedule(env, slice);
}

static size_t
h2o_nif_string_base64_encode_2_map(ErlNifEnv *env, h2o_nif_slice_t *slice, size_t offset, size_t length)
{
    size_t offset2 = slice->offset2;
    int url_encoded = slice->flags;
    size_t outlen;
    size_t enclen;

    while (length % 3 != 0) {
        length++;
    }

    if ((offset + length) >= slice->length) {
        length = slice->length - offset;
    }

    outlen = h2o_base64_encode_capacity(length);
    enclen = h2o_base64_encode((char *)(slice->out.data + offset2), (const char *)(slice->in.data + offset), length, url_encoded);

    slice->offset2 += (outlen && !url_encoded) ? outlen - 1 : enclen;

    return (offset + length);
}

static ERL_NIF_TERM
h2o_nif_string_base64_encode_2_reduce(ErlNifEnv *env, h2o_nif_slice_t *slice)
{
    ERL_NIF_TERM out;

    out = enif_make_binary(env, &slice->out);
    out = enif_make_sub_binary(env, out, 0, slice->offset2);

    return out;
}

static ERL_NIF_TERM
h2o_nif_string_base64_encode_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;
    int url_encoded;
    ErlNifBinary outbin;
    size_t outlen;

    if (argc != 2 || !enif_inspect_iolist_as_binary(env, argv[0], &in) || (argv[1] != ATOM_false && argv[1] != ATOM_true)) {
        return enif_make_badarg(env);
    }

    url_encoded = (argv[1] == ATOM_true) ? 1 : 0;
    outlen = h2o_base64_encode_capacity(in.size);

    if (in.size <= MAX_PER_SLICE) {
        ERL_NIF_TERM out;
        size_t enclen;
        if (!enif_alloc_binary(outlen, &outbin)) {
            return enif_make_badarg(env);
        }
        enclen = h2o_base64_encode((char *)outbin.data, in.data, in.size, url_encoded);
        out = enif_make_binary(env, &outbin);
        out = enif_make_sub_binary(env, out, 0, (outlen && !url_encoded) ? outlen - 1 : enclen);
        return out;
    }

    if (!enif_alloc_binary(outlen, &outbin)) {
        return enif_make_badarg(env);
    }
    h2o_nif_slice_t *slice = h2o_nif_slice_create(env, "string_base64_encode", in.size, 0, h2o_nif_string_base64_encode_2_map,
                                                  h2o_nif_string_base64_encode_2_reduce, NULL);
    slice->offset2 = 0;
    slice->flags = url_encoded;
    slice->in = in;
    slice->out = outbin;

    return h2o_nif_slice_schedule(env, slice);
}

static size_t
h2o_nif_string_hex_decode_1_map(ErlNifEnv *env, h2o_nif_slice_t *slice, size_t offset, size_t length)
{
    size_t offset2 = slice->offset2;
    size_t outlen;

    if (slice->flags == -1) {
        return slice->length;
    }

    while (length % 2 != 0) {
        length++;
    }

    if ((offset + length) >= slice->length) {
        length = slice->length - offset;
    }

    outlen = length / 2;

    slice->flags = h2o_hex_decode((char *)(slice->out.data + offset2), (const char *)(slice->in.data + offset), length);

    if (slice->flags == -1) {
        return slice->length;
    }

    slice->offset2 += outlen;

    return (offset + length);
}

static ERL_NIF_TERM
h2o_nif_string_hex_decode_1_reduce(ErlNifEnv *env, h2o_nif_slice_t *slice)
{
    if (slice->flags == -1) {
        (void)enif_release_binary(&slice->out);
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM out;
    out = enif_make_binary(env, &slice->out);
    return out;
}

static ERL_NIF_TERM
h2o_nif_string_hex_decode_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;
    ErlNifBinary outbin;
    size_t outlen;

    if (argc != 1 || !enif_inspect_iolist_as_binary(env, argv[0], &in) || (in.size % 2) != 0) {
        return enif_make_badarg(env);
    }

    outlen = in.size / 2;

    if (in.size <= MAX_PER_SLICE) {
        ERL_NIF_TERM out;
        if (!enif_alloc_binary(outlen, &outbin)) {
            return enif_make_badarg(env);
        }
        (void)h2o_hex_decode((char *)outbin.data, (const char *)in.data, in.size);
        out = enif_make_binary(env, &outbin);
        out = enif_make_sub_binary(env, out, 0, outlen);
        return out;
    }

    if (!enif_alloc_binary(outlen, &outbin)) {
        return enif_make_badarg(env);
    }
    h2o_nif_slice_t *slice = h2o_nif_slice_create(env, "string_hex_decode", in.size, 0, h2o_nif_string_hex_decode_1_map,
                                                  h2o_nif_string_hex_decode_1_reduce, NULL);
    slice->flags = 0;
    slice->in = in;
    slice->out = outbin;

    return h2o_nif_slice_schedule(env, slice);
}

static size_t
h2o_nif_string_hex_encode_1_map(ErlNifEnv *env, h2o_nif_slice_t *slice, size_t offset, size_t length)
{
    size_t offset2 = slice->offset2;
    size_t outlen = (length * 2) + 1;

    (void)h2o_hex_encode((char *)(slice->out.data + offset2), (const char *)(slice->in.data + offset), length);

    slice->offset2 += outlen - 1;

    return (offset + length);
}

static ERL_NIF_TERM
h2o_nif_string_hex_encode_1_reduce(ErlNifEnv *env, h2o_nif_slice_t *slice)
{
    ERL_NIF_TERM out;

    out = enif_make_binary(env, &slice->out);
    out = enif_make_sub_binary(env, out, 0, slice->offset2);

    return out;
}

static ERL_NIF_TERM
h2o_nif_string_hex_encode_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;
    ErlNifBinary outbin;
    size_t outlen;

    if (argc != 1 || !enif_inspect_iolist_as_binary(env, argv[0], &in)) {
        return enif_make_badarg(env);
    }

    outlen = (in.size * 2) + 1;

    if (in.size <= MAX_PER_SLICE) {
        ERL_NIF_TERM out;
        if (!enif_alloc_binary(outlen, &outbin)) {
            return enif_make_badarg(env);
        }
        (void)h2o_hex_encode((char *)outbin.data, in.data, in.size);
        out = enif_make_binary(env, &outbin);
        out = enif_make_sub_binary(env, out, 0, outlen - 1);
        return out;
    }

    if (!enif_alloc_binary(outlen, &outbin)) {
        return enif_make_badarg(env);
    }
    h2o_nif_slice_t *slice = h2o_nif_slice_create(env, "string_hex_encode", in.size, 0, h2o_nif_string_hex_encode_1_map,
                                                  h2o_nif_string_hex_encode_1_reduce, NULL);
    slice->offset2 = 0;
    slice->in = in;
    slice->out = outbin;

    return h2o_nif_slice_schedule(env, slice);
}

static size_t
h2o_nif_string_uri_escape_2_map(ErlNifEnv *env, h2o_nif_slice_t *slice, size_t offset, size_t length)
{
    char *preserve_chars = (char *)slice->data;
    size_t offset2 = slice->offset2;

    h2o_iovec_t outv = h2o_uri_escape(slice->pool, (const char *)(slice->in.data + offset), length, preserve_chars);
    (void)memcpy(slice->out.data + offset2, outv.base, outv.len);

    slice->offset2 += outv.len;

    (void)h2o_mem_clear_pool(slice->pool);

    return (offset + length);
}

static ERL_NIF_TERM
h2o_nif_string_uri_escape_2_reduce(ErlNifEnv *env, h2o_nif_slice_t *slice)
{
    ERL_NIF_TERM out;

    out = enif_make_binary(env, &slice->out);
    out = enif_make_sub_binary(env, out, 0, slice->offset2);

    return out;
}

static ERL_NIF_TERM
h2o_nif_string_uri_escape_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;
    ErlNifBinary preserve_chars_bin;
    char *preserve_chars;
    ErlNifBinary outbin;
    size_t outlen;

    if (argc != 2 || !enif_inspect_iolist_as_binary(env, argv[0], &in) ||
        (argv[1] != ATOM_false && !enif_inspect_iolist_as_binary(env, argv[1], &preserve_chars_bin))) {
        return enif_make_badarg(env);
    }

    if (argv[1] == ATOM_false || preserve_chars_bin.size == 0) {
        preserve_chars = NULL;
    } else {
        preserve_chars = (char *)enif_alloc(preserve_chars_bin.size + 1);
        (void)memset(preserve_chars, 0, preserve_chars_bin.size + 1);
        (void)memcpy(preserve_chars, preserve_chars_bin.data, preserve_chars_bin.size);
    }

    if (in.size <= MAX_PER_SLICE) {
        ERL_NIF_TERM out;
        h2o_iovec_t outv = h2o_uri_escape(NULL, (const char *)in.data, in.size, preserve_chars);
        unsigned char *buf = enif_make_new_binary(env, outv.len, &out);
        (void)memcpy(buf, outv.base, outv.len);
        (void)free(outv.base);
        return out;
    }

    outlen = in.size * 3;

    if (!enif_alloc_binary(outlen, &outbin)) {
        return enif_make_badarg(env);
    }
    h2o_nif_slice_t *slice = h2o_nif_slice_create(env, "string_uri_escape", in.size, 0, h2o_nif_string_uri_escape_2_map,
                                                  h2o_nif_string_uri_escape_2_reduce, NULL);
    slice->offset2 = 0;
    slice->in = in;
    slice->out = outbin;
    slice->data = (void *)preserve_chars;
    slice->pool = (h2o_mem_pool_t *)enif_alloc(sizeof(h2o_mem_pool_t));
    (void)memset(slice->pool, 0, sizeof(h2o_mem_pool_t));
    (void)h2o_mem_init_pool(slice->pool);

    return h2o_nif_slice_schedule(env, slice);
}

static ERL_NIF_TERM
h2o_nif_string_get_filext_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary path;

    if (argc != 1 || !enif_inspect_iolist_as_binary(env, argv[0], &path)) {
        return enif_make_badarg(env);
    }

    h2o_iovec_t filext = h2o_get_filext((const char *)path.data, (path.size > MAX_PER_SLICE) ? MAX_PER_SLICE : path.size);

    ERL_NIF_TERM out;

    if (filext.len) {
        if (enif_is_binary(env, argv[0])) {
            out = enif_make_sub_binary(env, argv[0], ((unsigned char *)filext.base) - path.data, filext.len);
        } else {
            unsigned char *buf = enif_make_new_binary(env, filext.len, &out);
            (void)memcpy(buf, filext.base, filext.len);
        }
    } else {
        (void)enif_make_new_binary(env, 0, &out);
    }

    return out;
}

/* WARN: blocking possible, unsafe */
static ERL_NIF_TERM
h2o_nif_string_str_stripws_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;

    if (argc != 1 || !enif_inspect_iolist_as_binary(env, argv[0], &in)) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM out;
    h2o_iovec_t stripws = h2o_str_stripws((const char *)in.data, in.size);
    if (stripws.len) {
        if (enif_is_binary(env, argv[0])) {
            out = enif_make_sub_binary(env, argv[0], ((unsigned char *)stripws.base) - in.data, stripws.len);
        } else {
            unsigned char *buf = enif_make_new_binary(env, stripws.len, &out);
            (void)memcpy(buf, stripws.base, stripws.len);
        }
    } else {
        (void)enif_make_new_binary(env, 0, &out);
    }

    return out;
}

static size_t
h2o_nif_string_htmlescape_1_map(ErlNifEnv *env, h2o_nif_slice_t *slice, size_t offset, size_t length)
{
    size_t offset2 = slice->offset2;

    if (slice->flags == -1) {
        return slice->length;
    }

    h2o_iovec_t outv = h2o_htmlescape(slice->pool, (const char *)(slice->in.data + offset), length);

    slice->offset2 += outv.len;

    if (slice->offset2 > slice->out.size) {
        if (!enif_realloc_binary(&slice->out, slice->offset2 + (slice->length - offset - length))) {
            slice->flags = -1;
            (void)h2o_mem_clear_pool(slice->pool);
            return slice->length;
        }
    }

    (void)memcpy(slice->out.data + offset2, outv.base, outv.len);
    (void)h2o_mem_clear_pool(slice->pool);

    return (offset + length);
}

static ERL_NIF_TERM
h2o_nif_string_htmlescape_1_reduce(ErlNifEnv *env, h2o_nif_slice_t *slice)
{
    if (slice->flags == -1) {
        (void)enif_release_binary(&slice->out);
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM out;
    out = enif_make_binary(env, &slice->out);
    return out;
}

static ERL_NIF_TERM
h2o_nif_string_htmlescape_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;
    ErlNifBinary outbin;
    size_t outlen;

    if (argc != 1 || !enif_inspect_iolist_as_binary(env, argv[0], &in)) {
        return enif_make_badarg(env);
    }

    if (in.size <= MAX_PER_SLICE) {
        h2o_mem_pool_t pool;
        (void)h2o_mem_init_pool(&pool);
        ERL_NIF_TERM out;
        h2o_iovec_t outv = h2o_htmlescape(&pool, (const char *)in.data, in.size);
        if (outv.base == NULL) {
            return enif_make_badarg(env);
        }
        unsigned char *buf = enif_make_new_binary(env, outv.len, &out);
        (void)memcpy(buf, outv.base, outv.len);
        (void)h2o_mem_clear_pool(&pool);
        return out;
    }

    outlen = in.size;

    if (!enif_alloc_binary(outlen, &outbin)) {
        return enif_make_badarg(env);
    }
    h2o_nif_slice_t *slice = h2o_nif_slice_create(env, "string_htmlescape", in.size, 0, h2o_nif_string_htmlescape_1_map,
                                                  h2o_nif_string_htmlescape_1_reduce, NULL);
    slice->offset2 = 0;
    slice->flags = 0;
    slice->in = in;
    slice->out = outbin;
    slice->pool = (h2o_mem_pool_t *)enif_alloc(sizeof(h2o_mem_pool_t));
    (void)memset(slice->pool, 0, sizeof(h2o_mem_pool_t));
    (void)h2o_mem_init_pool(slice->pool);

    return h2o_nif_slice_schedule(env, slice);
}
