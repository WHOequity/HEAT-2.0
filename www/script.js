
// © Copyright World Health Organization (WHO) 2016.
// This file is part of the Health Equity Assessment Toolkit (HEAT).
// HEAT is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License Version 2 as published by
// the Free Software Foundation.

// HEAT is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
// You should have received a copy of the GNU General Public License
// along with HEAT. If not, see http://www.gnu.org/licenses/.

var justifyColumns = function (chart) {
    var categoriesWidth = chart.plotSizeX / (1 + chart.xAxis[0].max - chart.xAxis[0].min),
        distanceBetweenColumns = 0,
        each = Highcharts.each,
        sum, categories = chart.xAxis[0].categories,
        number;
    for (var i = 0; i < categories.length; i++) {
        sum = 0;
        each(chart.series, function (p, k) {
            if (p.visible) {
                each(p.data, function (ob, j) {
                    if (ob.category == categories[i]) {
                        sum++;
                    }
                });
            }
        });
        distanceBetweenColumns = categoriesWidth / (sum + 1);
        number = 1;
        each(chart.series, function (p, k) {
            if (p.visible) {
                each(p.data, function (ob, j) {
                    if (ob.category == categories[i]) {
                        ob.graphic.element.x.baseVal.value = i * categoriesWidth + distanceBetweenColumns * number - ob.pointWidth / 2;
                        number++;
                    }
                });
            }
        });
    }
};


$(document).ready(function(){
    $('[data-toggle="tooltip"]').tooltip({
        placement : 'top'
    });

//http://stackoverflow.com/questions/14248194/close-responsive-navbar-automatically
$('.navbar-collapse a').click(function(){
    $(".navbar-collapse").collapse('hide');
});


      $('.dropdown').addClass('pull-right');

});


