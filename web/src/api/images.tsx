import 'whatwg-fetch';
import {
    Image,
    DeleteTagsRequest,
    GetImageResponse,
    GetTagsResponse,
    PostImageRequest,
    PostTagsRequest
} from './types';

const getImages = async (): Promise<GetImageResponse> => {
    const request = {
        headers: { 'Authorization': '96033b4a-44b0-4c14-ac44-890330b9877e' },
        method:  'GET',
    };

    const response  = await fetch('http://127.0.0.1:3001/api/image', request);
    const json      = await response.json();
    return await json;
}

const deleteTags = async (uuid: string, req: DeleteTagsRequest): Promise<void> => {
    const request = {
        headers: { 'Authorization': '96033b4a-44b0-4c14-ac44-890330b9877e' },
        method:  'POST',
    };

    fetch(`http://127.0.0.1:3001/api/image/${uuid}/tags`, request);
}

const getTags = async (uuid: string): Promise<GetTagsResponse> => {
    const request = {
        headers: { 'Authorization': '96033b4a-44b0-4c14-ac44-890330b9877e' },
        method:  'GET',
    };

    const response = await fetch(`http://127.0.0.1:3001/api/image/${uuid}/tags`, request);
    const json     = await response.json();
    return await json;
}

const uploadImage = async (req: PostImageRequest): Promise<void> => {
    const request = {
        headers: { 'Authorization': '96033b4a-44b0-4c14-ac44-890330b9877e' },
        method:  'POST',
    };

    fetch('http://127.0.0.1:3001/api/image', request);
}

const uploadTags = async (uuid: string, req: PostTagsRequest): Promise<void> => {
    const request = {
        headers: { 'Authorization': '96033b4a-44b0-4c14-ac44-890330b9877e' },
        method:  'POST',
    };

    fetch('http://127.0.0.1:3001/api/image', request);
}

export {
    getImages,
    deleteTags,
    getTags,
    uploadImage,
    uploadTags
}
