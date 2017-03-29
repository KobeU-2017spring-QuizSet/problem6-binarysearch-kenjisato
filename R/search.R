
binary_search <- function(haystack, needle) {
  n <- length(haystack)
  if (n == 1) {
    if (haystack == needle) {
      return(1)
    } else {
      message("Cannot find the needle in the haystack.")
      return(NA)
    }
  }
  
  mid = n %/% 2
  if (haystack[mid] == needle) {
    return(mid)
  } else if (haystack[mid] < needle) {
    return(mid + binary_search(haystack[(mid + 1):n], needle))
  } else {
    return(binary_search(haystack[1:(mid - 1)], needle))
  }
}


# Tests ----

test_binary_search <- function() {
  
  oopt <- options(message = FALSE); on.exit(options(oopt))
  
  stopifnot(
    # Simple test
    binary_search(haystack = 1:10, needle = 5) == 5,
    # Non existent
    is.na(suppressMessages(binary_search(haystack = 1:10, needle = 11)))
  )

  # A more complicated test
  
  haystack <- sort(sample(1:99, replace = TRUE))
  needle <- haystack[80]
  
  stopifnot(
    haystack[binary_search(haystack, needle)] == needle
  )
}

test_binary_search()