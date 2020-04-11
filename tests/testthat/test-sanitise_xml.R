test_that("sanitise_xml() escapes comparison operators for a single equation", {
  test_text <- '
  <variables>
    <aux name = "var1">
      <eqn>
        IF_THEN_ELSE(a > b :AND: a <= c , a, 0)
      </eqn>
    </aux>
  </variables>'

  actual_output <- sanitise_xml(test_text)

  expected_output <- '
  <variables>
    <aux name = "var1">
      <eqn>
        IF_THEN_ELSE(a &gt; b :AND: a &lt;= c , a, 0)
      </eqn>
    </aux>
  </variables>'

  expect_equal(actual_output, expected_output)
})

test_that("sanitise_xml() escapes comparison operators for multiple equation", {
  test_text <- '
  <variables>
    <aux name = "var1">
      <eqn>
        IF_THEN_ELSE(a > b :AND: a <= c , a, 0)
      </eqn>
    </aux>
    <aux name = "var2">
      <eqn>
        IF_THEN_ELSE(b > a :AND: c <= a , 0, a)
      </eqn>
    </aux>
    <aux name = "var3">
      <eqn>
        IF_THEN_ELSE(b < a :AND: c >= a , b, a)
      </eqn>
    </aux>
  </variables>'

  actual_output <- sanitise_xml(test_text)

  expected_output <- '
  <variables>
    <aux name = "var1">
      <eqn>
        IF_THEN_ELSE(a &gt; b :AND: a &lt;= c , a, 0)
      </eqn>
    </aux>
    <aux name = "var2">
      <eqn>
        IF_THEN_ELSE(b &gt; a :AND: c &lt;= a , 0, a)
      </eqn>
    </aux>
    <aux name = "var3">
      <eqn>
        IF_THEN_ELSE(b &lt; a :AND: c &gt;= a , b, a)
      </eqn>
    </aux>
  </variables>'

  expect_equal(actual_output, expected_output)
})

test_that("sanitise_xml() does not alter the original xml code", {
  test_text <- '
    <model>
      <variables>
        <aux name = "var1">
          <eqn>a + b</eqn>
        </aux>
        <aux name = "var2">
          <eqn>a + c</eqn>
        </aux>
        <aux name = "var3">
          <eqn>b + c</eqn>
        </aux>
      </variables>
    </model>'

  actual_output <- sanitise_xml(test_text)

  expected_output <- '
    <model>
      <variables>
        <aux name = "var1">
          <eqn>a + b</eqn>
        </aux>
        <aux name = "var2">
          <eqn>a + c</eqn>
        </aux>
        <aux name = "var3">
          <eqn>b + c</eqn>
        </aux>
      </variables>
    </model>'

  expect_equal(actual_output, expected_output)
})


