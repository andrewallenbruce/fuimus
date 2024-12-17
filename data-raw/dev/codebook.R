# Codebook

## Code

```{r}
#| label: code
code <- \(variable, levels, labels) {
  dplyr::tibble(
    variable = variable,
    level = levels,
    label = labels)
}

code(
  "SEX",
  levels = c(0, 1),
  labels = c("Male", "Female"))
```

## Codebook

```{r}
#| label: codebook
codebook <- \(...) {

  map <- rlang::list2(...)

  names(map) <- purrr::map_chr(map, \(x) x$variable[1])

  return(map)
}

codebook(
  code("SEX",
       levels = 0:1,
       labels = c("Male", "Female")),
  code("STUDY",
       levels = 1:3,
       labels = c("S1", "S2", "S3")))
```

## Encode

```{r}
#| label: encode
encode <- \(data, codebook) {

  mapped_vars <- names(codebook)

  mapped_data <- purrr::map2(data, names(data), \(x, var, m) {

    if (!var %in% mapped_vars) return(x)

    x <- factor(x, levels = m[[var]]$level, labels = m[[var]]$label)

    return(x)
  },
  m <- codebook
  )
  dplyr::as_tibble(mapped_data)
}

labels <- codebook(
  code("SEX",
       levels = 0:1,
       labels = c("Male", "Female")),
  code("STUDY",
       levels = 1:3,
       labels = c("S1", "S2", "S3"))
)

dataset <- data.frame(
  STUDY = c(1, 1, 1, 2, 2, 2),
  SEX   = c(0, 0, 1, 1, 1, 0),
  AGE   = c(32, 18, 64, 52, 26, 80)
)

encode(dataset, labels)
```

## Decode

```{r}
#| label: decode
decode <- \(data, codebook) {

  mapped_vars <- names(codebook)

  mapped_data <- purrr::map2(data, names(data), \(x, var, m) {

    if (!var %in% mapped_vars) return(x)

    x_chr <- as.character(x)

    x_lvl <- m[[var]]$level

    x_lab <- m[[var]]$label

    x <- x_lvl[match(x_chr, x_lab)]

    return(x)
  },
  m <- codebook
  )
  dplyr::as_tibble(mapped_data)
}

labels <- codebook(
  code("SEX", levels = c(0, 1), labels = c("Male", "Female")),
  code("STUDY", levels = c(1, 2, 3), labels = c("S1", "S2", "S3")))

dataset <- data.frame(
  STUDY = factor(c("S1", "S1", "S1", "S2", "S2", "S2")),
  SEX   = factor(c("Male", "Male", "Female", "Female", "Female", "Male")),
  AGE   = c(32, 18, 64, 52, 26, 80))

decode(dataset, labels)
```
