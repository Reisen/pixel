import React              from 'react';
import styles             from './ImagePanel.module.css';
import { Image as image } from '../../../../api/types';

interface Props {
    image: image
}

const ImagePanel = (props: Props) => {
    return (
        <img
            className={styles.Root}
            src={props.image.path}
            alt="Focused"
        />
    );
}

export default ImagePanel;
