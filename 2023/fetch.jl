import Downloads
using Dates

function main()
    url = "https://adventofcode.com/2023/leaderboard/private/view/$(ENV["AOC_ID"]).json"
    headers = ["cookie" => "session=$(ENV["AOC_SESSION"])"]
    utc = now()
    start = ceil(utc, Hour(1))

    println("Hourly fetch starts in $(canonicalize(start - utc)) ($(start - utc))")
    sleep((start - utc).value / 1000)

    while true
        postfix = Dates.format(now(), dateformat"yyyy-mm-ddTH")
        output = "leaderboard_$(ENV["AOC_ID"])_$(postfix).json"
        Downloads.download(url, output; headers = headers)
        utc = now()
        start = ceil(utc, Hour(1))
        sec = (start - utc).value / 1000
        println("Next fetch starts in $(canonicalize(start - utc)) ($(sec)s)")
        sleep(sec)
    end
end

main()
