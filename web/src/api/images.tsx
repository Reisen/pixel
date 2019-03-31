import 'whatwg-fetch';
import Cookies from 'universal-cookie';
import {
    Image,
    DeleteTagsRequest,
    GetImageResponse,
    GetTagsResponse,
    PostImageRequest,
    PostTagsRequest
} from './types';

// HACK, BIG HACK
const findApiBase = () => {
    const cookies = new Cookies();
    return cookies.get('api') || 'http://127.0.0.1:3001';
};

const getImages = async (): Promise<GetImageResponse> => {
    const request = {
        headers: { 'Authorization': '96033b4a-44b0-4c14-ac44-890330b9877e' },
        method:  'GET',
    };

    const base      = findApiBase();
    const response  = await fetch(`${base}/api/image`, request);
    const json      = await response.json();
    return await json;
}

const deleteTags = async (uuid: string, req: DeleteTagsRequest): Promise<void> => {
    const request = {
        headers: { 'Authorization': '96033b4a-44b0-4c14-ac44-890330b9877e' },
        method:  'POST',
    };

    const base = findApiBase();
    fetch(`${base}/api/image/${uuid}/tags`, request);
}

const getTags = async (uuid: string): Promise<GetTagsResponse> => {
    const request = {
        headers: { 'Authorization': '96033b4a-44b0-4c14-ac44-890330b9877e' },
        method:  'GET',
    };

    const base     = findApiBase();
    const response = await fetch(`${base}/api/image/${uuid}/tags`, request);
    const json     = await response.json();
    return await json;
}

const uploadImage = async (req: FormData): Promise<Response> => {
    const request = {
        headers: { 'Authorization': '96033b4a-44b0-4c14-ac44-890330b9877e' },
        method:  'POST',
        body:    req
    };

    const base = findApiBase();
    return fetch(`${base}/api/image`, request);
}

const uploadTags = async (uuid: string, req: PostTagsRequest): Promise<void> => {
    const request = {
        headers: { 'Authorization': '96033b4a-44b0-4c14-ac44-890330b9877e' },
        method:  'POST',
    };

    const base = findApiBase();
    fetch(`${base}/api/image`, request);
}

export {
    getImages,
    deleteTags,
    getTags,
    uploadImage,
    uploadTags
}
