import React              from 'react';
import Cookies            from 'universal-cookie';
import styles             from './CommentFeed.module.css';
import { Image as image } from '../../../../api/types';


// HACK, BIG HACK
const findApiBase = () => {
    const cookies = new Cookies();
    return cookies.get('base') || 'http://localhost:3000';
};

interface Props {
    image: image
}

const CommentFeed = (props: Props) => {
    return (
        <img
            className={styles.Root}
            src={`${findApiBase()}/${props.image.path}`}
            alt="Focused"
        />
    );
}

export default CommentFeed;
