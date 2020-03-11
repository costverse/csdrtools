
csdr <- left_join(ff$Data, dictionary, by = c("WBSElementID"))

# Add a column to identify if each WBSElementID is a lowest level WBS or not.
csdr <- left_join(csdr, {wbs_expand(csdr) %>% distinct(wbs, is_lowest)},
                  by = c("WBSElementID" = "wbs"))

csdr <- csdr %>% select(-WBSElementName.y)

csdr <- csdr %>% rename("WBSElementName" = WBSElementName.x)