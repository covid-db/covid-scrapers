require 'mechanize'
require 'json'
require 'csv'

LANDING_URL = 'https://e.infogram.com/_/iYFQBLyILrbrGYNcJs3d'
HEADERS = %w[county confirmed deaths recovered]
DATA_REGEX = /window.infographicData=(\{.+\})/

# initialize csv with date stamp
date = Date.today.strftime('%Y_%m_%d')
csv_name = 'kcra_california_counties_%s.csv' % date

# initialize crawler and hit endpoint
agent = Mechanize.new
page = agent.get(LANDING_URL)

# parse out data
infographic = page.at('script[3]').children[0]
mt = DATA_REGEX.match(infographic)
json = JSON.parse(mt[1])
data = json['elements'][4]['data'][0]

clean_data = data.map { |row| row.drop(4) }

county_data = clean_data.map do |county, *numbers|
  numbers.map! { |str| /\d+/.match(str)[0] }
  numbers.prepend(county)
end

CSV.open(csv_name, 'w+') do |csv|
  csv << HEADERS if csv.count == 0
  county_data.each { |county| csv << county }
end
