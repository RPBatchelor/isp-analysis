


library(rvest)

aemo_page <- "https://aemo.com.au/en/energy-systems/electricity/national-electricity-market-nem/nem-forecasting-and-planning/forecasting-and-planning-data/generation-information"


html <- read_html(aemo_page)


file_list <- html |> 
  html_nodes("a") |>
  html_attr("href") |> 
  as_tibble() |> 
  rename(html_data = value) |> 
  mutate(excel_file = str_detect(html_data, ".xlsx")) |> 
  filter(excel_file == T) |> 
  mutate(download_link = paste0("www.aemo.com.au", html_data))
  




  map(~ tibble(href = html_text(html_node(.x, ".download"), "href")))





file_link <- webpage %>%
  html_nodes("a[title='Current file']") %>%  # Select the <a> tag with the title "Current file"
  html_attr("href")                          # Extract the href attribute

# Convert relative URL to absolute URL
file_link <- url_absolute(file_link, base = webpage_url)

file_link



#content > div > div > div > div > div > div.col-12.col-lg-8.layout-col > div:nth-child(6) > div > ul > li > a
//*[@id="content"]/div/div/div/div/div/div[2]/div[6]/div/ul/li/a



sess <- read_html_live("https://www.bodybuilding.com/exercises/finder")
sess$view()

sess %>% html_elements(".ExResult-row") %>% length()
sess$click(".ExLoadMore-btn")
sess %>% html_elements(".ExResult-row") %>% length()
sess$click(".ExLoadMore-btn")
sess %>% html_elements(".ExResult-row") %>% length()




html <- read_html(url)

html %>%
  html_nodes("tr") %>% 
  map(~ tibble(type = html_text(html_node(., ".filing-type"), T),
               href = html_attr(html_node(., ".download"), "href")
  )) %>% 
  bind_rows() %>% 
  filter(type == "AA")





url <- "https://beta.companieshouse.gov.uk/company/02280000/filing-history"

html <- read_html(url)

html %>%
  html_nodes("tr") %>% 
  map(~ tibble(type = html_text(html_node(., ".filing-type"), T),
               href = html_attr(html_node(., ".download"), "href")
  )) %>% 
  bind_rows() %>% 
  filter(type == "AA")


<a title="Current file" href="/-/media/files/electricity/nem/planning_and_forecasting/generation_information/2024/nem-generation-information-july-2024.xlsx?la=en" target="_blank"><div class="file-list-wrapper"><div class="field-updated field-publisheddate"><span>29/07/2024</span></div><h5 class="field-title">July 2024 Generation Information</h5><div class="field-filetypeicon"><span class="xlsx"></span></div><div class="field-size"><span>962.32&nbsp;KB</span></div><h5 class="success field-linktext"></h5></div></a>