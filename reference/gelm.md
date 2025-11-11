# `getelem` with more flexibility

`getelem` with more flexibility

## Usage

``` r
gelm(l, e, m = "re", ...)
```

## Arguments

- l:

  named `<list>`

- e:

  `<chr>` element name; can be a regex pattern if `m` is `"re"`

- m:

  `<chr>` mode; default is `"re"` (regex) or `"df"` (data frame)

- ...:

  additional arguments to pass to
  [`collapse::get_elem()`](https://sebkrantz.github.io/collapse/reference/extract_list.html)

## Value

`<chr>` or `<data.frame>`, depending on `m`
