date_index=0
av_wind = 1
max_wind = 2
wind_dir = 3
daily_avg = 4
daily_max = 5
av_dir = 6


daily_data = []

with open('cayleyWindRaw.csv', 'r') as fullCSV:
    line = fullCSV.readline()
    titles = line.split(',')



    line = fullCSV.readline().strip('\n').split(",")
    index = 0
    while len(line) == 4:
        date = line[0].split(" ")[0];
        daily_data.append([[], [], [], []])
        daily_data[index][date_index]=date
        while line[0].split(" ")[0] == date:
            daily_data[index][av_wind].append(float(line[av_wind]))
            daily_data[index][max_wind].append(float(line[max_wind]))
            daily_data[index][wind_dir].append(float(line[wind_dir]))

            line = fullCSV.readline().strip('\n').split(",")

        index=index+1
        #line = fullCSV.readline().strip('\n').split(",")

    for day in daily_data:
        day.append(sum(day[av_wind])/len(day[av_wind])) #add average wind over the day
        day.append(max(day[max_wind])) # add maximum wind over the day
        day.append(sum(day[wind_dir])/len(day[wind_dir])) # add maximum wind over the day


    for day in daily_data:
        print(day[date_index] + " "+ str(day[daily_avg]) + " "+str(day[daily_max]) + " "+str(day[av_dir]) )



    with open('cayley_wind.csv', 'w') as outCSV:
        outCSV.write('date,daily_avg,daily_max,avg_dir\n')
        for day in daily_data:
            outCSV.write(day[date_index] + ","+ str(day[daily_avg]) + ","+str(day[daily_max]) + ","+str(day[av_dir])+'\n')


    min_max_wind = min([x[daily_max] for x in daily_data])
    max_max_wind = max([x[daily_max] for x in daily_data])

    min_avg_wind = min([x[daily_avg] for x in daily_data])
    max_avg_wind = max([x[daily_avg] for x in daily_data])

    min_direction = 0
    max_direction = 3600

    for day in daily_data:
        day[daily_avg] = (day[daily_avg] - min_avg_wind) / (max_avg_wind - min_avg_wind)
        day[daily_max] = (day[daily_max] - min_max_wind) / (max_max_wind - min_max_wind)
        day[av_dir] = (day[av_dir])/360

    with open('cayley_wind_normed.csv', 'w') as outCSV:
        outCSV.write('date,daily_avg,daily_max,avg_dir\n')
        for day in daily_data:
            outCSV.write(day[date_index] + ","+ str(day[daily_avg]) + ","+str(day[daily_max]) + ","+str(day[av_dir])+'\n')
