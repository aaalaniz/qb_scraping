from scrapy.spider import BaseSpider
from scrapy.selector import HtmlXPathSelector
import re

from qb_scraper.items import QbSalaryItem

class QbSalarySpider(BaseSpider):
    name = "spotrac"
    allowed_domains = ["localhost"]
    start_urls = [
        "file://localhost/Users/aaalaniz/development/data_mining/qb_scraper/nfl_2006_draft_rookies.html",
        "file://localhost/Users/aaalaniz/development/data_mining/qb_scraper/nfl_2007_draft_rookies.html",
        "file://localhost/Users/aaalaniz/development/data_mining/qb_scraper/nfl_2008_draft_rookies.html",
        "file://localhost/Users/aaalaniz/development/data_mining/qb_scraper/nfl_2009_draft_rookies.html",
        "file://localhost/Users/aaalaniz/development/data_mining/qb_scraper/nfl_2010_draft_rookies.html",
        "file://localhost/Users/aaalaniz/development/data_mining/qb_scraper/nfl_2011_draft_rookies.html",
        "file://localhost/Users/aaalaniz/development/data_mining/qb_scraper/nfl_2012_draft_rookies.html"
    ]
    count = 0
    years = [
        "2006",
        "2007",
        "2008",
        "2009",
        "2010",
        "2011",
        "2012"
    ]

    def parse(self, response):
        hxs = HtmlXPathSelector(response)
        stat_ref = []
        items = []

        # Get the rows
        players = hxs.select('//table/tbody/tr[contains(@class, "visible players")]')

        # Get the stats
        for player in players:
            item = QbSalaryItem()
            item['year'] = self.years[self.count]
            stats = player.select('td')
            stat_count = 0
            for stat in stats:
                if len(stat.select('a').select('text()').extract()) == 0:
                    stat = str(stat.select('text()').extract())
                    stat = re.search('\'(.*)\'', stat)
                    if stat == None:
                        stat = "-999"
                    else:
                        stat = stat.group(1)
                else:
                    stat = str(stat.select('a').select('text()').extract())
                    stat = re.search('\'(.*)\'', stat)
                    if stat == None:
                        stat = "-999"
                    else:
                        stat = stat.group(1)
                if stat_count == 0:
                    foo = stat
                    stat = re.search('(\d+)(\.|\..*\')(\d+)', stat)
                    if stat != None:
                        item['dr_round'] = stat.group(1)
                        item['dr_overall'] = stat.group(3)
                    else:
                        print(foo)
                        item['dr_round'] = stat
                        item['dr_overall'] = stat
                if stat_count == 1:
                    name = stat.split(',')
                    if len(name) > 1:
                        name = name[1] + " " + name[0]
                        item['name'] = name.replace('.','')
                    else:
                        item['name'] = stat
                if stat_count == 5:
                    item['contract_len'] = stat
                if stat_count == 6:
                    stat = stat.replace(',', '')
                    item['contract_total'] = stat
                if stat_count == 8:
                    stat = stat.replace(',', '')
                    item['avg_salary'] = stat

                stat_count = stat_count + 1
            items.append(item)

        self.count = self.count + 1
        return items
