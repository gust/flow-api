angular.module("flowApi", ['ui.router', 'ui.bootstrap.collapse']);


angular.module('flowApi').config(['$stateProvider', function($stateProvider){
  $stateProvider.state('releases',{
    url: '/releases',
    views: {
      'main': {
        templateUrl: '/templates/releases.html',
        controllerAs: "releaseCtrl",
        controller: [ 'releaseResource', function(releaseResource){
          var _this = this;
          releaseResource.all().then(function(result){
            console.log("Retreived releases", result);
            _this.releases = result.data;
          });
        }]
      }
    }
  });
}]);

angular.module('flowApi').factory('releaseResource',['$http', function($http){
  releaseResource = {};
  releaseResource.all = function(){
    return $http.get('/releases');
  }
  return releaseResource;
}]);

angular.module('flowApi').directive('storytypelabel',[function(){
  return {
    restrict: 'E',
    replace: true,
    template: "<span class='label pull-right'> {{ storytype }}</span",
    scope: {
      storyType: '@'
    },
    link: function(scope, elem, attrs, ctrl){
      console.log("type", attrs)
      scope.storytype = attrs.storytype;
      switch(attrs.storytype){
        case "feature" :
          elem.addClass("label-success")
          break;
        case "bug" :
          elem.addClass("label-warning")
          break;
        case "chore" :
          elem.addClass("label-info")
          break;
      }
    }
  }
}]);
