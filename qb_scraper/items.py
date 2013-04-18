# Define here the models for your scraped items
#
# See documentation in:
# http://doc.scrapy.org/topics/items.html

from scrapy.item import Item, Field

class QbItem(Item):
    name = Field()
    height = Field()
    weight = Field()
    age = Field()
    year = Field()
    team = Field()
    pos = Field()
    number = Field()
    games_played = Field()
    games_started = Field()
    rec_in_gs = Field()
    completions = Field()
    att = Field()
    completion_percentage = Field()
    yds = Field()
    tds= Field()
    td_percentage = Field()
    ints =  Field()
    int_percentage = Field()
    longest_pass = Field()
    yds_per_att = Field()
    avg_yds_per_att = Field()
    yds_per_comp = Field()
    yds_per_game = Field()
    rating = Field()
    qbr = Field()
    sacked = Field()
    yds_lost_by_sack = Field()
    net_yds_per_att = Field()
    adj_net_yds_per_att = Field()
    perc_times_sk = Field()
    comebacks = Field()
    gwds = Field()
    avg_value = Field()

class QbSalaryItem(Item):
    name = Field()
    year = Field()
    dr_round = Field()
    dr_overall = Field()
    contract_len = Field()
    contract_total = Field()
    avg_salary = Field()

class CfbQbItem(Item):
	name = Field()
	cmpp = Field()
	att = Field()
	pct = Field()
	yds = Field()
	ya = Field()
	aya = Field()
	td = Field()
	inter = Field()
	rate = Field()
	numyrs = Field()
