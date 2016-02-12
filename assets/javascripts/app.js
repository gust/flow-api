angular.module("flowApi", ['ui.router', 'ui.bootstrap.collapse', 'ui.bootstrap.pagination', 'uib/template/pagination/pagination.html']);

angular.module('flowApi').config(['$stateProvider', function($stateProvider){
  $stateProvider
  .state('releases',{
    url: '/releases',
    abstract: true,
    template: "<section ui-view='main'></section",
  })
  .state('releases.index',{
    url: '',
    views: {
      'main': {
        templateUrl: '/templates/releases.html',
        controllerAs: "releasesCtrl",
        controller: [ 'releaseResource', '$scope', function(releaseResource, $scope){
          var _this = this;

          this.pageChanged = function(){
            getReleases({ page: _this.currentPage });
          }
          getReleases({});
          function getReleases(params) {
            releaseResource.all(params).then(function(result){
              _this.totalItems = result.data.pagination.totalResults;
              _this.currentPage = result.data.pagination.page;
              _this.perPage = result.data.pagination.perPage;
              _this.releases = result.data.releases;
            });
          }
        }]
      }
    }
  })
  .state('releases.show',{
    url: '/:id',
    views: {
      'main': {
        templateUrl: '/templates/release.html',
        controllerAs: "releaseCtrl",
        controller: [ 'releaseResource', '$stateParams', function(releaseResource, $stateParams){
          var _this = this;
          var id = $stateParams.id;
          releaseResource.find(id).then(function(result){
            _this.release = result.data[0];
          });
        }]
      }
    }
  });
}]);

angular.module('flowApi').factory('releaseResource',['$http', function($http){
  releaseResource = {};
  releaseResource.all = function(params){
    return $http.get('/releases', {  params: (params || {}) });
  };

  releaseResource.find = function(id){
    return $http.get('/releases/' + id);
  };
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

angular.module('flowApi').directive('release',[function(){
  return {
    restrict: 'EA',
    replace: true,
    templateUrl: "/templates/release_data.html",
    scope: {
      releaseData: '='
    }
  }
}]);
