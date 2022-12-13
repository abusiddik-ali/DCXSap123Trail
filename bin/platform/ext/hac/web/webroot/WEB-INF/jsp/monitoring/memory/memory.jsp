<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %>
<html>
<head>
    <title>Memory</title>

    <link rel="stylesheet" type="text/css" href="<c:url value="/static/css/chartjs/Chart.min.css"/>"/>

    <script type="text/javascript" src="<c:url value="/static/js/history.js"/>"></script>
    <script type="text/javascript" src="<c:url value="/static/js/monitoring/memory.js?i=1"/>"></script>
    <script type="text/javascript" src="<c:url value="/static/js/chartjs/Chart.min.js"/>"></script>
    <script type="text/javascript">
        Chart.platform.disableCSSInjection = true;
    </script>


</head>
<body>
<div class="prepend-top span-17 colborder" id="content">
    <button id="toggleSidebarButton">&gt;</button>
    <div class="marginLeft" id="charts" data-chartDataUrl="<c:url value="/monitoring/memory/data/"/>">
        <h2>Memory</h2>
        <div id="warningMessage" style="text-align:center;"></div>
        <div id="chart" style="text-align:center; width: 500px; height: 300px">
            <canvas id="chart-area" width="500" height="300"></canvas>
        </div>
        <div id="chart2" style="text-align:center; width: 500px; height: 300px">
            <canvas id="chart2-area" width="500" height="300"></canvas>
        </div>
        <button id="gcButton" data-url="<c:url value="/monitoring/memory/gc/"/>">Run Garbage Collector</button>
    </div>
</div>
<div class="span-6 last" id="sidebar">
    <div class="prepend-top" id="recent-reviews">
        <h3 class="caps">
            Page description
        </h3>
        <div class="box">
            <div class="quiet">
                This page provides status parameters of the Java Virtual Machine memory. You can run Garbage Collector
                to clear memory and refresh statistics.
            </div>
        </div>
        <h3 class="caps">
            Useful Links
        </h3>
        <div class="box">
            <ul>
                <li><a href="${wikiPerformance}" target="_blank" rel="noopener noreferrer" class="quiet">Performance Tuning</a></li>
            </ul>
        </div>
    </div>
</div>
</body>
</html>

