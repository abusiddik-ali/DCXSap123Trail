/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
queryMap = [:]

def parseLine(line) {

    def parts = line.split('\\|')

    def query
    def kind = parts[4]

    def timeMatcher = parts[3] =~ /^(\d+)/

    def queryTime = Integer.parseInt(timeMatcher[0][1])

    if (kind =~ /statement/) {
        if (parts[5].size() > 0)
            query = parts[5]
        else // no prepared statement taking sql
            query = parts[6]
    } else {
        query = parts[4] // for commit, rollback etc.
    }

    if (queryMap[query]) {
        queryMap[query].count++
        queryMap[query].time += queryTime
    } else {
        queryMap[query] = [count: 1, time: queryTime]
    }
}

def flushBuffer(lineBuffer) {

    if (lineBuffer.size() > 0) {
        parseLine(lineBuffer.join("\n"))
        lineBuffer.clear()
    }
}

// main

def logFile = new File(jdbcFile)
def lineBuffer = new LinkedList()

logFile.eachLine { line, lineNumber ->

    if (line =~ /^\d+\|[^\|]*\|[^\|]*\|[^\|]*\|[^\|]*\|/) { // line starts with 5 tokens separeted by 4 pipes

        flushBuffer(lineBuffer)
        lineBuffer.add(line)

    } else {

        lineBuffer.add(line)
    }
}

flushBuffer(lineBuffer)

//output

def totalQueries = queryMap.collect { it.value.count }.inject(0) { sum, item -> sum + item }
def totalTime = queryMap.collect { it.value.time }.inject(0) { sum, item -> sum + item }

queryMap = queryMap.sort { a, b -> b.value.time <=> a.value.time }

result = [
        queryMap    : queryMap,
        totalQueries: totalQueries,
        totalTime   : totalTime
]
