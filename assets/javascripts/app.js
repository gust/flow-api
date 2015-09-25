angular.module("flowApi", ['ui.router']);


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
