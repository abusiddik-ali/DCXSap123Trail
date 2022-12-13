
var freeMemoryChart;
var heapChart;

$(document).ready(function () {
    freeMemoryChart = new Chart($('#chart-area'), {
        type: 'pie',
        data: {
            datasets: [{
                data: [],
                backgroundColor: ["#3366cc", "#dc3912"],
                label: "Memory"
            }],
            labels: []
        },
        options: {
            legend: {
                position: 'right',
            },
            tooltips: {
                callbacks: {
                    label: function (tooltipItem, data) {
                        var dataset = data.datasets[tooltipItem.datasetIndex];
                        var meta = dataset._meta[Object.keys(dataset._meta)[0]];
                        var total = meta.total;
                        var currentValue = dataset.data[tooltipItem.index];
                        var percentage = parseFloat((currentValue / total * 100).toFixed(1));
                        return currentValue + ' (' + percentage + '%)';
                    },
                    title: function (tooltipItem, data) {
                        return data.labels[tooltipItem[0].index];
                    }
                },
                backgroundColor: "rgb(255,255,255,0.95)",
                titleFontColor: "rgb(0,0,0)",
                bodyFontColor: "rgb(0,0,0)"
            }
        }
    });

    heapChart = new Chart($('#chart2-area'), {
        type: 'bar',
        data: {
            datasets: [
                {
                    label: 'init',
                    backgroundColor: "#3366cc"
                },
                {
                    label: 'used',
                    backgroundColor: "#dc3912"
                }
                ,
                {
                    label: 'committed',
                    backgroundColor: "#ff9900"
                },
                {
                    label: 'max',
                    backgroundColor: "#109618"
                }
            ],
            labels: ['heap', 'non-heap']
        },
        options: {
            layout: {
                padding: {
                    left: 30,
                    top: 80
                }
            },
            tooltips: {
                backgroundColor: "rgb(255,255,255,0.95)",
                titleFontColor: "rgb(0,0,0)",
                bodyFontColor: "rgb(0,0,0)"
            },
            legend: {
                position: 'right',
            }
        }
    });
    getData();
    window.setInterval(getData, 15000)
});


$(document).ready(function () {
    var token = $("meta[name='_csrf']").attr("content");

    $("body").on('click', '#gcButton', function () {
        var url = $('#gcButton').attr('data-url');
        $.ajax({
            url: url,
            type: 'POST',
            headers: {
                'Accept': 'application/json',
                'X-CSRF-TOKEN': token
            },
            success: function (data) {
                drawCircleChart(data);
                drawColumnChart(data);
            },
            error: hac.global.err
        });

    });
});

function getData() {
    var token = $("meta[name='_csrf']").attr("content");

    debug.log("Get Data...");
    var url = $('#charts').attr('data-chartDataUrl');
    $.ajax({
        url: url,
        type: 'GET',
        headers: {
            'Accept': 'application/json',
            'X-CSRF-TOKEN': token
        },
        success: function (data) {
            drawCircleChart(data);
            drawColumnChart(data);
        },
        error: hac.global.err
    });

}

function drawCircleChart(d) {
    debug.log(d.freeMemory + " / " + d.usedMemory + " / " + d.totalMemory);

    debug.log(freeMemoryChart);
    freeMemoryChart.data.datasets[0].data = [d.freeMemory, d.usedMemory];
    freeMemoryChart.data.labels = ['Free', 'Used'];
    freeMemoryChart.update();
}

function drawColumnChart(d) {
    heapChart.data.datasets[0].data = [d.heap_init, d.nonheap_init];
    heapChart.data.datasets[1].data = [d.heap_used, d.nonheap_used];
    heapChart.data.datasets[2].data = [d.heap_comitted, d.nonheap_comitted];
    heapChart.data.datasets[3].data = [d.heap_max, d.nonheap_max];
    heapChart.update();
}

