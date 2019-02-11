import Image     from '../Image';
import Pager     from '../Pager';
import React     from 'react';
import styles    from './ImageGrid.module.css';
import { image } from '../../../../types/image';

interface Props {
    images: image[];
    width: number;
}

const renderValidImages = (images: image[]) => images.map(image =>
    <Image resolution="800x600" />
);

const renderEmptyImages = (total: number, width: number) => {
    const emptyCount = width - (total % width);
    const fillSlice  = Array(emptyCount).fill(0);
    return fillSlice.map(x =>
        <Image empty />
    );
}

const ImageGrid = (props: Props) => (
    <div className={styles.ImageGrid}>
        { renderValidImages(props.images) }
        { renderEmptyImages(props.images.length, props.width) }

        <Pager
          current={1}
          total={Math.floor(props.images.length / 24)}
        />
    </div>
);

export default ImageGrid;
