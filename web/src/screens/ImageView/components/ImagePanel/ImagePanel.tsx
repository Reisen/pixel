import React     from 'react';
import Cookies   from 'universal-cookie';
import styles    from './ImagePanel.module.css';
import { image } from '../../../../types/image';


// HACK, BIG HACK
const findApiBase = () => {
    const cookies = new Cookies();
    return cookies.get('base') || 'http://localhost:3000';
};

interface Props {
    image: image
}

const ImagePanel = (props: Props) => {
    return (
        <img
            className={styles.Root}
            src={`${findApiBase()}/${props.image.path}`}
            alt="Focused"
        />
    );
}

export default ImagePanel;
