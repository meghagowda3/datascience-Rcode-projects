library(rvest)
library(XML)
library(magrittr)

# Amazon Reviews
aurl <- "http://www.amazon.in/Apple-MMGF2HN-13-3-inch-Integrated-Graphics/product-reviews/B01FUK9TKG/ref=cm_cr_arp_d_paging_btm_2?showViewpoints=1&pageNumber"
amazon_reviews <- NULL
for (i in 1:20){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
write.table(amazon_reviews,"apple.txt")


# Snapdeal reviews 
surl_1 <- "https://www.snapdeal.com/product/samsung-galaxy-J3-8gb-4g/676860597612/ratedreviews?page="
surl_2 <- "&sortBy=HELPFUL&ratings=4,5#defRevPDP"
snapdeal_reviews <- NULL
for (i in 1:20){
  surl <- read_html(as.character(paste(surl_1,surl_2,sep=as.character(i))))
  srev <- surl %>%
    html_nodes("#defaultReviewsCard p") %>%
    html_text()
  snapdeal_reviews <- c(snapdeal_reviews,srev)
}

write.table(snapdeal_reviews,"samsung.txt")
getwd()