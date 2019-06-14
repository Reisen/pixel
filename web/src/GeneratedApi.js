import axios from 'axios';

export const postApiImage = function(body, headerCookie)
{
  return axios({ url: '/api/image'
    , method: 'post'
    , data: body
    , responseType: 'json'
    , withCredentials: true
    });
}



export const getApiImage = function(headerCookie)
{
  return axios({ url: '/api/image'
    , method: 'get'
    , withCredentials: true
    });
}



export const getApiImageByUuid = function(uuid, headerAuthorization)
{
  return axios({ url: '/api/image/' + encodeURIComponent(uuid) + ''
    , method: 'get'
    , withCredentials: true
    });
}



export const getApiImageByUuidTags = function(uuid, headerAuthorization)
{
  return axios({ url: '/api/image/' + encodeURIComponent(uuid) + '/tags'
    , method: 'get'
    , withCredentials: true
    });
}



export const postApiImageByUuidTags = function(uuid, body, headerCookie)
{
  return axios({ url: '/api/image/' + encodeURIComponent(uuid) + '/tags'
    , method: 'post'
    , data: body
    , responseType: 'json'
    , withCredentials: true
    });
}



export const deleteApiImageByUuidTags = function(uuid, body, headerCookie)
{
  return axios({ url: '/api/image/' + encodeURIComponent(uuid) + '/tags'
    , method: 'delete'
    , data: body
    , responseType: 'json'
    , withCredentials: true
    });
}



export const postApiUserLogin = function(body)
{
  return axios({ url: '/api/user/login'
    , method: 'post'
    , data: body
    , responseType: 'json'
    , withCredentials: true
    });
}



export const postApiUser = function(body)
{
  return axios({ url: '/api/user'
    , method: 'post'
    , data: body
    , responseType: 'json'
    , withCredentials: true
    });
}
