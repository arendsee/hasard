context("flow.R")

test_that(
  "Compositions",
  {
    f <- function( ) 'f'          
    g <- function(x) paste(x,'g') 
    h <- function(x) paste(x,'h') 

    expect_equal(compose(f,g,h)(), 'f g h')

    expect_error(compose(1,2))

    htype(f) <- 'NA -> a'
    htype(g) <- 'a  -> b'
    htype(h) <- 'b  -> NA'

    expect_true(are_composable(f,g))
    expect_true(are_composable(g,h))
    expect_false(are_composable(f,h))
    expect_false(are_composable(h,f))

    expect_null(compose())
    expect_true(is.function(compose(f)))

    expect_equal(htype(compose(f,g)), c(NA, 'b'))
    expect_true(all(is.na(htype(compose(f,g,h)))))

    expect_error(compose(h,f))
    expect_error(compose(f,f))
    expect_error(compose(g,f))
  }
)

test_that(
  "test connect",
  {

    A <- hwell('a')
    B <- hpipe('a -> b')
    C <- hpipe('b -> c')
    D <- hpipe('b -> c -> d')

    # h_fun(A) <- function() 'a'
    # h_fun(B) <- function(x) paste0(x, 'b')
    # h_fun(C) <- function(x) paste0(x, 'c')
    # h_fun(D) <- function(x,y) sprintf('(%s)(%s)d', x, y)
    #
    # expect_null(connect('A'))
    # expect_equal((connect('A --> B')), 'A --> B')
    # expect_equal((connect('A --> B --> C')), c('A --> B', 'B --> C'))
    # expect_equal((connect('B C --> D')), 'B C --> D')
    # expect_equal((connect('(A --> B) (A --> B --> C) --> D')), c('A --> B', 'A --> B', 'B --> C', 'B C --> D'))
    #
    # # Bad expression
    # expect_error(connect('((A --> B'))
    # # Type error (should be caught in `h_inode<-`)
    # expect_error(connect('B --> A'))

    D1 <- connect('(A --> B) (A --> B --> C) --> D')
    print(ls(D1))
    print(D1)
    print(D1$D__7)
    print(with(D1, D__7()))
    # print(eval(D1$D__7(), D1))
    # expect_equal(with(D1, D__7()), '(ab)(abc)d')

    # # Check for weird name handling
    # a_.1 <- hwell('a')
    # ._1A <- hpipe('a -> b')
    # expect_equal((connect('a_.1 --> ._1A')), 'a_.1 --> ._1A')
  }
)
