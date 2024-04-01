"OpenAPI Generator"
===================

This is a command line tool that allows you to generate OCaml code for and inspect OpenAPI
specifications.

## Usage

The generator makes a number of assumptions that, if held true, should result in
*functional* generated code, even if it's not ideal (e.x. types may be Jsonaf.t instead
of a reasonable typed OCaml type in all cases).

1. The request / response formats are JSON. (Currently XML, and even plain text are not
supported.)
2. The spec is OpenAPI V3.0.3. Some V3.0.0 specs may work. A V3.1.0 spec is less
likely to work because OpenAPI doesn't follow SemVer, but *may* also work. A V2.X.X spec
will almost definitely not work.
3. (I think that's it? If you discover a new and fun failure mode you should document it
here.)

### Generating Code

If your spec is in YAML, just convert it to JSON first before putting it in this tool.

```bash
$ openapi.exe generate -name example_generated -out $CODE_OUTPUT_PATH -spec $SPEC_JSON
```

The generated code requires you to pass in a `Openapi_runtime.Client.t`, which can be created with any arbitrary backend (Httpaf, Cohttp, etc.).

### Inspecting Routes

```bash
$ openapi.exe inspect paths -spec $SPEC_JSON
/v1/foobar
GET: Obtain foobar
POST: Create foobar

/v1/foobar/{baz}
GET: Fetch baz
DELETE: Delete baz

/v1/foobar/{baz}/import
POST: Import baz
...
```

### Inspecting Inferred Types

It's probably easier to just generate the OCaml code and look at the types that way,
but this gives you a summary of some internal state of the type inference engine, which
may be useful for debugging.

```bash
$ openapi.exe inspect types -spec $SPEC_JSON
((name Foo_request) (structure (Record ((id 0))))
 (variant_name ()))
((name Bar_submit) (structure (Record ((id 3) (baz_count 4))))
 (variant_name ()))
((name Error)
 (structure (Record ((message 7) (id 8))))
 (variant_name ()))
 ...
```
