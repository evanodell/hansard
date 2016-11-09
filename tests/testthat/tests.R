

test_that("publication_logs returns a data frame with a positive length", {

  pub.data <- publication_logs()

  expect_is(pub.data, "data.frame")
  expect_gte(length(pub.data)>0)
})


test_that("constituencies returns a data frame with a positive length", {

  cont.data <- constituencies()

  expect_is(cont.data, "data.frame")
  expect_gte(length(cont.data)>0)
})


test_that("members returns a data frame with a positive length", {

  member.data <- members("all")

  expect_is(member.data, "data.frame")
  expect_gte(length(member.data)>0)
})



#lords_ammendments


#election_results
#elections

#commons_oral_question_times

#constituencies
