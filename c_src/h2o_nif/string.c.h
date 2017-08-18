// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "../slice.h"

/* fun h2o_nif:string_tolower/1 */

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

/* fun h2o_nif:string_strtolower/1 */

static size_t
h2o_nif_string_strtolower_1_map(ErlNifEnv *env, h2o_nif_slice_t *slice, size_t offset, size_t length)
{
    (void)h2o_strtolower((char *)(slice->out.binary.data + offset), length);
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

    h2o_nif_slice_t *slice = NULL;
    if (!h2o_nif_slice_create("string_strtolower", h2o_nif_string_strtolower_1_map, NULL, &slice)) {
        return enif_make_badarg(env);
    }
    slice->in.length = in.size;
    if (!enif_alloc_binary(in.size, &slice->out.binary)) {
        (void)h2o_nif_slice_release(slice);
        return enif_make_badarg(env);
    }
    (void)memcpy(slice->out.binary.data, in.data, in.size);

    return h2o_nif_slice_schedule(env, slice);
}

/* fun h2o_nif:string_toupper/1 */

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

/* fun h2o_nif:string_strtoupper/1 */

static size_t
h2o_nif_string_strtoupper_1_map(ErlNifEnv *env, h2o_nif_slice_t *slice, size_t offset, size_t length)
{
    (void)h2o_strtoupper((char *)(slice->out.binary.data + offset), length);
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

    h2o_nif_slice_t *slice = NULL;
    if (!h2o_nif_slice_create("string_strtoupper", h2o_nif_string_strtoupper_1_map, NULL, &slice)) {
        return enif_make_badarg(env);
    }
    slice->in.length = in.size;
    if (!enif_alloc_binary(in.size, &slice->out.binary)) {
        (void)h2o_nif_slice_release(slice);
        return enif_make_badarg(env);
    }
    (void)memcpy(slice->out.binary.data, in.data, in.size);

    return h2o_nif_slice_schedule(env, slice);
}

/* fun h2o_nif:string_lcstris/2 */

static size_t
h2o_nif_string_lcstris_2_map(ErlNifEnv *env, h2o_nif_slice_t *slice, size_t offset, size_t length)
{
    int result = slice->flags;
    if (result == 0) {
        return slice->in.length;
    }
    result = h2o_lcstris((const char *)slice->in.binary.data, slice->in.binary.size, (const char *)slice->out.binary.data,
                         slice->out.binary.size);
    if (result) {
        return (offset + length);
    } else {
        slice->flags = result;
        return slice->in.length;
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

    h2o_nif_slice_t *slice = NULL;
    if (!h2o_nif_slice_create("string_lcstris", h2o_nif_string_lcstris_2_map, h2o_nif_string_lcstris_2_reduce, &slice)) {
        return enif_make_badarg(env);
    }
    slice->flags = 1;
    slice->in.length = target.size;
    slice->in.binary = target;
    slice->out.binary = test;

    return h2o_nif_slice_schedule(env, slice);
}

/* fun h2o_nif:string_base64_encode_capacity/1 */

static ERL_NIF_TERM
h2o_nif_string_base64_encode_capacity_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned int len;

    if (argc != 1 || !enif_get_uint(env, argv[0], &len)) {
        return enif_make_badarg(env);
    }

    return enif_make_uint(env, h2o_base64_encode_capacity(len));
}

/* fun h2o_nif:string_decode_base64url/1 */

static size_t
h2o_nif_string_decode_base64url_1_map(ErlNifEnv *env, h2o_nif_slice_t *slice, size_t offset, size_t length)
{
    size_t i_pos = offset;
    size_t o_pos = slice->out.offset;

    h2o_iovec_t outv = h2o_decode_base64url(&slice->pool, (const char *)(slice->in.binary.data + i_pos), length);
    if (outv.base == NULL) {
        slice->badarg = 1;
        (void)enif_release_binary(&slice->out.binary);
        return slice->in.length;
    }
    (void)memcpy((slice->out.binary.data + o_pos), outv.base, outv.len);
    slice->out.offset += outv.len;

    (void)h2o_mem_clear_pool(&slice->pool);

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
    h2o_nif_slice_t *slice = NULL;
    if (!h2o_nif_slice_create("string_decode_base64url", h2o_nif_string_decode_base64url_1_map, NULL, &slice)) {
        (void)enif_release_binary(&outbin);
        return enif_make_badarg(env);
    }
    slice->in.length = in.size;
    slice->in.binary = in;
    slice->out.binary = outbin;

    return h2o_nif_slice_schedule(env, slice);
}

/* fun h2o_nif:string_base64_encode/2 */

static size_t
h2o_nif_string_base64_encode_2_map(ErlNifEnv *env, h2o_nif_slice_t *slice, size_t offset, size_t length)
{
    size_t i_pos = offset;
    size_t o_pos = slice->out.offset;
    int url_encoded = slice->flags;
    size_t outlen;
    size_t enclen;

    while (length % 3 != 0) {
        length++;
    }

    if ((offset + length) >= slice->in.length) {
        length = slice->in.length - offset;
    }

    outlen = h2o_base64_encode_capacity(length);
    enclen = h2o_base64_encode((char *)(slice->out.binary.data + o_pos), (const char *)(slice->in.binary.data + i_pos), length,
                               url_encoded);

    slice->out.offset += (outlen && !url_encoded) ? outlen - 1 : enclen;

    return (offset + length);
}

static ERL_NIF_TERM
h2o_nif_string_base64_encode_2_reduce(ErlNifEnv *env, h2o_nif_slice_t *slice)
{
    ERL_NIF_TERM out;

    out = enif_make_binary(env, &slice->out.binary);
    out = enif_make_sub_binary(env, out, 0, slice->out.offset);

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
    h2o_nif_slice_t *slice = NULL;
    if (!h2o_nif_slice_create("string_base64_encode", h2o_nif_string_base64_encode_2_map, h2o_nif_string_base64_encode_2_reduce,
                              &slice)) {
        (void)enif_release_binary(&outbin);
        return enif_make_badarg(env);
    }
    slice->flags = url_encoded;
    slice->in.length = in.size;
    slice->in.binary = in;
    slice->out.binary = outbin;

    return h2o_nif_slice_schedule(env, slice);
}

/* fun h2o_nif:string_hex_decode/1 */

static size_t
h2o_nif_string_hex_decode_1_map(ErlNifEnv *env, h2o_nif_slice_t *slice, size_t offset, size_t length)
{
    size_t i_pos = offset;
    size_t o_pos = slice->out.offset;
    size_t outlen;

    while (length % 2 != 0) {
        length++;
    }

    if ((offset + length) >= slice->in.length) {
        length = slice->in.length - offset;
    }

    outlen = length / 2;

    if (h2o_hex_decode((char *)(slice->out.binary.data + o_pos), (const char *)(slice->in.binary.data + i_pos), length) == -1) {
        slice->badarg = 1;
        (void)enif_release_binary(&slice->out.binary);
        return slice->in.length;
    }

    slice->out.offset += outlen;

    return (offset + length);
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
    h2o_nif_slice_t *slice = NULL;
    if (!h2o_nif_slice_create("string_hex_decode", h2o_nif_string_hex_decode_1_map, NULL, &slice)) {
        (void)enif_release_binary(&outbin);
        return enif_make_badarg(env);
    }
    slice->in.length = in.size;
    slice->in.binary = in;
    slice->out.binary = outbin;

    return h2o_nif_slice_schedule(env, slice);
}

/* fun h2o_nif:string_hex_encode/1 */

static size_t
h2o_nif_string_hex_encode_1_map(ErlNifEnv *env, h2o_nif_slice_t *slice, size_t offset, size_t length)
{
    size_t i_pos = offset;
    size_t o_pos = slice->out.offset;
    size_t outlen = (length * 2) + 1;

    (void)h2o_hex_encode((char *)(slice->out.binary.data + o_pos), (const char *)(slice->in.binary.data + i_pos), length);

    slice->out.offset += outlen - 1;

    return (offset + length);
}

static ERL_NIF_TERM
h2o_nif_string_hex_encode_1_reduce(ErlNifEnv *env, h2o_nif_slice_t *slice)
{
    ERL_NIF_TERM out;

    out = enif_make_binary(env, &slice->out.binary);
    out = enif_make_sub_binary(env, out, 0, slice->out.offset);

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
    h2o_nif_slice_t *slice = NULL;
    if (!h2o_nif_slice_create("string_hex_encode", h2o_nif_string_hex_encode_1_map, h2o_nif_string_hex_encode_1_reduce, &slice)) {
        (void)enif_release_binary(&outbin);
        return enif_make_badarg(env);
    }
    slice->in.length = in.size;
    slice->in.binary = in;
    slice->out.binary = outbin;

    return h2o_nif_slice_schedule(env, slice);
}

/* fun h2o_nif:string_uri_escape/2 */

typedef struct h2o_nif_string_uri_escape_2_s h2o_nif_string_uri_escape_2_t;
struct h2o_nif_string_uri_escape_2_s {
    h2o_nif_slice_t super;
    char *preserve_chars;
};

static size_t
h2o_nif_string_uri_escape_2_map(ErlNifEnv *env, h2o_nif_slice_t *super, size_t offset, size_t length)
{
    h2o_nif_string_uri_escape_2_t *slice = (h2o_nif_string_uri_escape_2_t *)super;
    size_t i_pos = offset;
    size_t o_pos = slice->super.out.offset;

    h2o_iovec_t outv =
        h2o_uri_escape(&slice->super.pool, (const char *)(slice->super.in.binary.data + i_pos), length, slice->preserve_chars);
    (void)memcpy(slice->super.out.binary.data + o_pos, outv.base, outv.len);

    slice->super.out.offset += outv.len;

    (void)h2o_mem_clear_pool(&slice->super.pool);

    return (offset + length);
}

static ERL_NIF_TERM
h2o_nif_string_uri_escape_2_reduce(ErlNifEnv *env, h2o_nif_slice_t *super)
{
    h2o_nif_string_uri_escape_2_t *slice = (h2o_nif_string_uri_escape_2_t *)super;
    ERL_NIF_TERM out;

    out = enif_make_binary(env, &slice->super.out.binary);
    out = enif_make_sub_binary(env, out, 0, slice->super.out.offset);
    if (slice->preserve_chars != NULL) {
        (void)mem_free(slice->preserve_chars);
        slice->preserve_chars = NULL;
    }

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
        preserve_chars = (char *)mem_alloc(preserve_chars_bin.size + 1);
        (void)memset(preserve_chars, 0, preserve_chars_bin.size + 1);
        (void)memcpy(preserve_chars, preserve_chars_bin.data, preserve_chars_bin.size);
    }

    if (in.size <= MAX_PER_SLICE) {
        ERL_NIF_TERM out;
        h2o_iovec_t outv = h2o_uri_escape(NULL, (const char *)in.data, in.size, preserve_chars);
        unsigned char *buf = enif_make_new_binary(env, outv.len, &out);
        (void)memcpy(buf, outv.base, outv.len);
        (void)free(outv.base);
        if (preserve_chars != NULL) {
            (void)mem_free(preserve_chars);
            preserve_chars = NULL;
        }
        return out;
    }

    outlen = in.size * 3;

    if (!enif_alloc_binary(outlen, &outbin)) {
        return enif_make_badarg(env);
    }
    h2o_nif_string_uri_escape_2_t *slice = NULL;
    if (!__h2o_nif_slice_create(sizeof(h2o_nif_string_uri_escape_2_t), "string_uri_escape", h2o_nif_string_uri_escape_2_map,
                                h2o_nif_string_uri_escape_2_reduce, (h2o_nif_slice_t **)&slice)) {
        (void)enif_release_binary(&outbin);
        if (preserve_chars != NULL) {
            (void)mem_free(preserve_chars);
            preserve_chars = NULL;
        }
        return enif_make_badarg(env);
    }
    slice->super.in.length = in.size;
    slice->super.in.binary = in;
    slice->super.out.binary = outbin;
    slice->preserve_chars = preserve_chars;

    return h2o_nif_slice_schedule(env, (h2o_nif_slice_t *)slice);
}

/* fun h2o_nif:string_get_filext/1 */

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

/* fun h2o_nif:string_str_stripws/1 */

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

/* fun h2o_nif:string_htmlescape/1 */

static size_t
h2o_nif_string_htmlescape_1_map(ErlNifEnv *env, h2o_nif_slice_t *slice, size_t offset, size_t length)
{
    size_t i_pos = offset;
    size_t o_pos = slice->out.offset;

    h2o_iovec_t outv = h2o_htmlescape(&slice->pool, (const char *)(slice->in.binary.data + i_pos), length);

    if (outv.base == NULL) {
        slice->badarg = 1;
        (void)enif_release_binary(&slice->out.binary);
        return slice->in.length;
    }

    slice->out.offset += outv.len;

    if (slice->out.offset > slice->out.binary.size) {
        if (!enif_realloc_binary(&slice->out.binary, slice->out.offset + (slice->in.length - i_pos - length))) {
            slice->badarg = 1;
            (void)enif_release_binary(&slice->out.binary);
            return slice->in.length;
        }
    }

    (void)memcpy(slice->out.binary.data + o_pos, outv.base, outv.len);
    (void)h2o_mem_clear_pool(&slice->pool);

    return (offset + length);
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
    h2o_nif_slice_t *slice = NULL;
    if (!h2o_nif_slice_create("string_htmlescape", h2o_nif_string_htmlescape_1_map, NULL, &slice)) {
        (void)enif_release_binary(&outbin);
        return enif_make_badarg(env);
    }
    slice->in.length = in.size;
    slice->in.binary = in;
    slice->out.binary = outbin;

    return h2o_nif_slice_schedule(env, slice);
}
