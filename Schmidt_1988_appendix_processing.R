library(googlesheets4)
library(tidyverse)

source("Schmidt_1988_gsheet.R")

schmidt_app <- googlesheets4::read_sheet(schmidt_sheet, sheet = 1, col_types = "icccccccccc")
schmidt_app

# footnote 143 from Schmidt's (1988) English translation (p. 28 in the translation):
## Note from Bernd Nothofer, August 2020: according to Schmidt, column 1986 contains Schmidt’s transcriptions of Nothofer’s cassette recordings. Whenever Schmidt’s and Nothofer’s transcriptions differ, he lists Nothofer’s after N:

schmidt_app1 <- schmidt_app %>% 
			   mutate(form_1986 = str_replace_all(form_1986, ",(\\sN)", ";\\1")) %>%
			   extract(col = form_1986, into = "nothofer_form", regex = "([(]?N.\\s.+)", remove = FALSE) %>%
			   mutate(nothofer_form = str_replace_all(nothofer_form, "N.\\s", "")) %>%
			   mutate(nothofer_form = str_replace_all(nothofer_form, "([,;]\\s)", " \\1")) %>%
			   mutate(schmidt_form = str_replace_all(form_1986, "([(]?N.\\s.+)", "")) %>%
			   mutate(schmidt_form = str_replace_all(schmidt_form, "([,;]\\s)", " \\1")) %>%
			   mutate(schmidt_form = str_trim(schmidt_form, side = "right")) %>%
			   mutate(schmidt_form = str_replace_all(schmidt_form, "\\s;$", "")) %>%
			   group_by(page, form_1937) %>%
			   # add row order for a given 1937's form
			   mutate(form1986_id = row_number())
			   
main_df <- schmidt_app1 %>% select(1:2, form_1986, form1986_id, schmidt_form, nothofer_form, everything())
main_df

# replace some characters ====
main_df1 <- main_df %>% 
	ungroup() %>% 
	mutate(across(matches("form|variant|crossref"), ~str_replace_all(., "'", "ˈ"))) %>% 
	mutate(across(matches("form|variant|crossref"), ~str_replace_all(., "ʔ", "ˀ"))) %>% 
	mutate(across(matches("form|variant|crossref"), ~str_replace_all(., ":", "ː")))
main_df1 %>% 
	mutate(across(where(is.character), ~replace_na(., ""))) %>%
	write_tsv("Schmidt_1988_appendix.tsv")