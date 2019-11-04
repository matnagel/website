{-# LANGUAGE OverloadedStrings #-}

module MiscPage (
miscPage
) where

import Utils

miscPage :: Html
miscPage = page "Miscellaneous" $ do
        menuBlock
        pageTitle "Miscellaneous"
        headline "Seminars"
        link "https://math.ethz.ch/news-and-events/events/research-seminars.html" "Seminars at ETH"
        headline "GDPR"
        p gdprNotice

gdprNotice :: Html
gdprNotice = do 
                "This website is static, and does not collect or process personal information.\
                \ Nevertheless, it is hosted on "
                link "https://pages.github.com" "Github pages"
                ". Github might collect and process further information your browser shares, which likely includes\
                \ your IP address. Please consult their "
                link "https://help.github.com/articles/github-privacy-statement/" "privacy statement"
                " for more information. Furthermore, some pages use "
                link "https://www.mathjax.org" "mathjax" 
                " to depict mathematical symbols, which is hosted on "
                link "https://www.cloudflare.com/privacypolicy/" "cloudflare"
                "."
